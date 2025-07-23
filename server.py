import asyncio
import os
import random
from aiohttp import web, WSMsgType
import json

COLOR_NAMES = [
    "Red", "Green", "Blue", "Cyan", "Magenta", "Yellow",
    "Black", "White", "Orange", "Purple", "Brown", "Pink", "Gray"
]

clients = {}

def makeMessage(mtype, mdata):
    return json.dumps( { "mtype": mtype, "mdata": mdata } )

async def websocket_handler(request):
    ws = web.WebSocketResponse()
    await ws.prepare(request)
    print("WebSocket connected")

    clients[ws] = {"active": False}

    try:
        async for msg in ws:
            if msg.type == WSMsgType.TEXT:

                ws_msg = json.loads(msg.data)

                ws_mtype = ws_msg["mtype"]

                print(f"Received: {ws_msg}")

                if ws_mtype == "cmd":
                    cmd = ws_msg["mdata"]

                    if cmd == "start":
                        clients[ws]["active"] = True
                        await ws.send_str(makeMessage("log", "Feed started."))
                    elif cmd == "stop":
                        clients[ws]["active"] = False
                        await ws.send_str(makeMessage("log", "Feed stopped."))
                    else:
                        await ws.send_str(makeMessage("log", "Unknown command."))

    except Exception as e:
        print(f"WebSocket error: {e}")
    finally:
        print("WebSocket disconnected")
        clients.pop(ws, None)

    return ws

async def broadcast_loop():
    while True:
        for ws, state in list(clients.items()):
            if state.get("active") and not ws.closed:
                color = random.choice(COLOR_NAMES)
                try:
                    await ws.send_str(makeMessage( "color", color ))
                    print(f"Sent to client: {color}")
                except Exception as e:
                    print(f"Send error: {e}")
        await asyncio.sleep(2)

async def main():
    app = web.Application()
    app.router.add_get('/api/ws', websocket_handler)
    app.router.add_static('/', path=os.path.abspath('dist'), show_index=True)

    runner = web.AppRunner(app)
    await runner.setup()
    site = web.TCPSite(runner, '127.0.0.1', 8080)
    await site.start()
    print("Server running at http://127.0.0.1:8080")

    await broadcast_loop()

if __name__ == '__main__':
    asyncio.run(main())

## ChatApp

TCP чат сервер и клиент


```chatserv``` - server application

```chatcli``` - client application

Каждый должен быть запущен из шелла вручную.


```chatcli_client:get_rooms_list()``` - Возвращает список айди доступных комнат

```chatcli_client:join_room(RoomId)``` -  Присоединиться к комнате (Пользователь должен быть членом комнаты перед тем как посылать в нее сообщения или менять имя)

```chatcli_client:set_name(RoomId, NewName)``` - Задать имя для комнаты

```chatcli_client:send_message(RoomId, MessageText)``` - Отправить новое сообщение в комнату


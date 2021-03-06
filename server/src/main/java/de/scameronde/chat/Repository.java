package de.scameronde.chat;

import java.util.List;
import java.util.Optional;

import de.scameronde.chat.businesstypes.ChatRoom;
import de.scameronde.chat.businesstypes.ChatMessageLog;
import de.scameronde.chat.businesstypes.Participant;

public interface Repository {
  String addParticipant(Participant participant);

  Optional<Participant> login(String participantName);

  List<ChatRoom> getChatRooms();

  String addChatRoom(ChatRoom chatRoom);

  void deleteChatRoom(ChatRoom chatRoom);

  void addChatMessage(ChatRoom chatRoom, String message, Participant participant);

  ChatMessageLog getChatMessageLog(ChatRoom chatRoom);
}

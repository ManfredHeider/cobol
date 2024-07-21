      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0178.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRAT0178.CPv  $
      *
      *   Rev 2.2   Feb 23 2000 17:38:26   T13EX44
      *PVW-Redesign
      *
      *   Rev 2.1   Jan 28 2000 11:28:16   T13EX44
      *Redesign PVW
      *
      *   Rev 2.0   Jul 08 1999 15:50:32   T13EX21
      *APA00039 - Neuerstellung
      *
      *   Rev 1.0   05 Sep 1997 09:21:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0178  KONSTANTEN FÜR BEARBEITUNGSMODUS   *
      *                           KTY-NR 142                         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    30.10.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !30.10.97! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      *         !        !    !                           !          *
      * apa00012!26.10.98! 02 ! Konstantenwerte vertauscht! Parent   *
      *         !        !    !                           !          *
      * APA00012!19.11.98! 03 ! Neuen Bearbeitungsmodus   ! Jäger    *
      *         !        !    ! KORRektur eingefügt       !          *
      * APA00063!22.11.99!2.01! NEUER MODUS "BATCH"       ! SCHEIDEL *
      *         !        !    ! BATCH-KURZ UND -LANG ENTF.!          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
221199     05 K-KNR-B-MODUS-BATCH      PIC S9(004) BINARY SYNC
221199                                             VALUE  1.
221199*    05 K-KNR-B-MODUS-BATCH-KURZ PIC S9(004) BINARY SYNC
221199*                                            VALUE  2.
221199*    05 K-KNR-B-MODUS-BATCH-LANG PIC S9(004) BINARY SYNC
221199*                                            VALUE  1.
191198     05 K-KNR-B-MODUS-BATCH-KORR PIC S9(004) BINARY SYNC
191198                                             VALUE  3.

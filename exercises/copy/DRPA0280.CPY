      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0280.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0280.CPv  $
      *
      *   Rev 2.0   26 Jul 1999 10:49:44   T01JM2S
      *APA00012 - NT-Umstellung
      *
      *   Rev 1.0   Apr 24 1997 14:30:06   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0280  PROVISIONS-DATEN AMV               *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                           *
      *    DATUM:    05.05.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!        ! 01 ! NEUERSTELLUNG             ! WOLFF    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE PROVISIONS-DATEN DER AMV.         *
      *                                                              *
      ****************************************************************
       01 DRPA0280-VORSATZ.
          05 DRPA0280-V-SATZART             PIC  X(001).
          05 DRPA0280-ABSENDER              PIC  X(007).
          05 DRPA0280-EMPFAENGER            PIC  X(004).
          05 DRPA0280-INHALT                PIC  X(021).
          05 DRPA0280-DATUM                 PIC  X(010).
       01 DRPA0280-NACHSATZ.
          05 DRPA0280-N-SATZART             PIC  X(001).
          05 DRPA0280-SATZANZAHL            PIC  9(006).
          05 DRPA0280-GUTSCHRIFTEN          PIC  9(008)V99.
          05 DRPA0280-BELASTUNGEN           PIC  9(008)V99.
       01 DRPA0280-DATENSATZ.
          10 DRPA0280-SATZART               PIC  X(001).
          COPY DRPA0295.
      *
      ******** Ende Copy-Member **************************************

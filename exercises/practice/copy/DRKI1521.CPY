      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1521.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/cpy/DRKI1521.CPv  $
      *
      *   Rev 2.2   May 09 2003 09:52:54   t13ex49
      *AKI00338 - nachträgliche Änderung des
      *Satzaufbaus für Rabatte
      *
      *   Rev 2.1   Apr 24 2003 09:16:34   t13ex49
      *AKI00338 - Änderung Rabattverarbeitung
      *
      *   Rev 2.0   Jan 13 2003 15:49:42   p13sp16
      *Archiverstellung
      *
      *--------------------------------------------------------------*
      *                                                              *
      *    COPY-MEMBER FÜR KFZ-DATEN AM-VERS Rabattdaten             *
      *    ---------------------------------------------             *
      *                                                              *
      *    SATZLÄNGE:   630                                          *
      *                                                              *
      *                                                              *
      *--------------------------------------------------------------*
      *    LOGBUCH                                                   *
      *--------------------------------------------------------------*
      *AUFTRAG !Datum     ! Änderung                        !Name    !
      *--------!----------!---------------------------------!--------!
      *AKI00338!06.03.2003!Neuerstellung                    !P. Pohl !
      *        !          !                                 !        !
      *--------!----------!---------------------------------!--------!
      *
       01  DRKI1521.

           05  VERS-Rab-Daten.

               10  VERS-Rab-GES                        PIC 9(02).
               10  VERS-Rab-VERTRNR                    PIC 9(15).
               10  VERS-Rab-RABATT-NR                  PIC 9(02).
               10  VERS-Rab-VERTRAGSID                 PIC X(13).
               10  VERS-Rab-VERTRAGS-VAKZ              PIC X(01).
               10  VERS-Rab-EUR-PROC-WKZ               PIC X(01).
080503         10  VERS-Rab-VAKZ                       PIC X(01).
080503*            I = Insert U = Update D = Delete
080503         10  VERS-Rab-ZN-TEXT                    PIC X(26).
080503         10  VERS-Rab-Einheit                    PIC 9(01).
080503         10  VERS-Rab-Wert                       PIC S9(08)V99.
080503         10  FILLER                              PIC X(20).

080503*        10  VERS-Rab-Tabelle                    OCCURS 10.

080503*            20  VERS-Rab-VAKZ                     PIC X(01).
      *                I = Insert U = Update D = Delete

080503*            20  VERS-Rab-ZN-TEXT                  PIC X(26).
080503*            20  VERS-Rab-Einheit                  PIC 9(01).
080503*            20  VERS-Rab-Wert                     PIC S9(08)V99.
080503*            20  FILLER                            PIC X(20).
      *
      ******** Tabellenende
080503*        10  FILLER                              PIC X(18).
      *
      *
      ******************************************************************
      *          E n d e    Copy-Member DRKI1521                       *
      ******************************************************************

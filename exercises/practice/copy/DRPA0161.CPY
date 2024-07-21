      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0161.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0161.CPv  $
      *
      *   Rev 2.2   Nov 29 1999 09:33:42   T13EX21
      *FPA00114 - RC-UNZULAESSIG korrigiert
      *
      *   Rev 2.1   Nov 25 1999 09:14:02   T13EX21
      *APA00063 - Neuerstellung
      *
      *   Rev 2.0   Feb 27 1999 14:28:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Feb 19 1997 10:21:58   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0161                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE 'DTA-BEARBEITUNG ABMELDEN' FÜR               *
      *       PROGRAMM UPPA0083 (DTA-SACHBEARBEITER)                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. STORCH                                        *
      *    DATUM:    06.10.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00063!06.10.99! 01 ! NEUERSTELLUNG             ! STORCH   *
      * FPA00114!29.11.99! 02 ! RC-UNZULAESSIG VON UZL IN ! STORCH   *
      *         !        !    ! UNZ GEÄNDERT              !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *
      ****************************************************************
      *
       01 PI-DRPA0161.
          05 PI-DRPA0161-PDTA-ID                 PIC S9(015)V COMP-3.
          05 PI-DRPA0161-KNR-DTA-BEARBFKT        PIC S9(004) COMP.
      *
       01 PO-DRPA0161.
          05 PO-DRPA0161-RC                     PIC  X(003).
             88 PO-DRPA0161-RC-OK                    VALUE 'OK '.
             88 PO-DRPA0161-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRPA0161-RC-UNZULAESSIG           VALUE 'UNZ'.

         05 PO-DRPA0161-DATEN.
      ***   INFOFELD: 5 ZEILEN A 80 ZEICHEN
            10 PO-DRPA0161-INFO.
               15 PO-DRPA0161-INFO-ZEILE-1         PIC  X(080).
               15 PO-DRPA0161-INFO-ZEILE-2         PIC  X(080).
               15 PO-DRPA0161-INFO-ZEILE-3         PIC  X(080).
               15 PO-DRPA0161-INFO-ZEILE-4         PIC  X(080).
               15 PO-DRPA0161-INFO-ZEILE-5         PIC  X(080).
      ******* ENDE COPY-MEMBER **************************************

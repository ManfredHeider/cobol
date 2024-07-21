      *--------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *     DATUM   :  NOVEMBER 1995
      *     KOPF AUS:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0055.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0055.CPv  $
      *
      *   Rev 2.1   Nov 09 2009 10:18:32   t13ex44
      *APA00820: Pflegedialog Rückstellung/Haftzeiten
      *
      *   REV 2.0   FEB 27 1999 14:27:30   P13SP20
      *NT-UMSTELLUNG
      *
      *   REV 1.1   MAR 06 1997 14:52:32   P13PVCS
      *NEUERSTELLUNG
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0055  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PRIGGEN                                         *
      *    DATUM:    20.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !20.08.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0055                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE <<<        PRCK TBPA0007                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0055.
          05 PI-DRPA0055-PRCK-LAST-UPDATE      PIC  X(026).
          05 PI-DRPA0055-PRCK-KEY.
             10 PI-DRPA0055-PROD-PROD-SCHL     PIC S9(4) USAGE COMP.
             10 PI-DRPA0055-PGEB-GES           PIC S9(4) USAGE COMP.
             10 PI-DRPA0055-PRCK-SELKRIT       PIC X(15).
             10 PI-DRPA0055-PRCK-GUELT-AB      PIC X(10).
          05 PI-DRPA0055-PRCK-DATEN.
             10 PI-DRPA0055-PRCK-GUELT-BIS     PIC X(10).
             10 PI-DRPA0055-KTY-HAFTZEITTYP    PIC S9(4) BINARY.
             10 PI-DRPA0055-KNR-HAFTZEITTYP    PIC S9(4) BINARY.
             10 PI-DRPA0055-PRCK-HAFTZT-PG     PIC S9(4) BINARY.
             10 PI-DRPA0055-PRCK-HAFTZT-VB     PIC S9(4) BINARY.

      *
       01 PO-DRPA0055.
          05 PO-DRPA0055-RC                    PIC  X(003).
             88 PO-DRPA0055-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0055-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA0055-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************

      *--------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *     DATUM   :  NOVEMBER 1995
      *     KOPF AUS:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0056.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0056.CPv  $
      *
      *   Rev 2.1   Nov 09 2009 10:18:38   t13ex44
      *APA00820: Pflegedialog Rückstellung/Haftzeiten
      *
      *   REV 2.0   FEB 27 1999 14:27:30   P13SP20
      *NT-UMSTELLUNG
      *
      *   REV 1.1   MAR 06 1997 14:52:52   P13PVCS
      *NEUERSTELLUNG
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0056  FUNKTION DELETE STANDARD-OZP       *
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
      *    FÜR DIE FUNKTION DRPA0056                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE <<<        PRCK TBPA0007                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0056.
          05 PI-DRPA0056-PRCK-LAST-UPDATE      PIC  X(026).
          05 PI-DRPA0056-PRCK-KEY.
             10 PI-DRPA0056-PROD-PROD-SCHL     PIC S9(4) USAGE COMP.
             10 PI-DRPA0056-PGEB-GES           PIC S9(4) USAGE COMP.
             10 PI-DRPA0056-PRCK-SELKRIT       PIC X(15).
             10 PI-DRPA0056-PRCK-GUELT-AB      PIC X(10).
      *
       01 PO-DRPA0056.
          05 PO-DRPA0056-RC                    PIC  X(003).
             88 PO-DRPA0056-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0056-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA0056-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************

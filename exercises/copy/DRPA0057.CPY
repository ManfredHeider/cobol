      *--------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *     DATUM   :  NOVEMBER 1995
      *     KOPF AUS:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0057.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0057.CPv  $
      *
      *   Rev 2.1   Nov 09 2009 10:18:42   t13ex44
      *APA00820: Pflegedialog Rückstellung/Haftzeiten
      *
      *   REV 2.0   FEB 27 1999 14:27:30   P13SP20
      *NT-UMSTELLUNG
      *
      *   REV 1.1   MAR 06 1997 14:53:00   P13PVCS
      *NEUERSTELLUNG
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0057  FUNKTION SELECT STANDARD-OZP       *
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
      *    FÜR DIE FUNKTION DRPA0057                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL <<<    PRCK TBPA0007                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0057.
          05 PI-DRPA0057-PRCK-KEY.
             10 PI-DRPA0057-PROD-PROD-SCHL     PIC S9(4) USAGE COMP.
             10 PI-DRPA0057-PGEB-GES           PIC S9(4) USAGE COMP.
             10 PI-DRPA0057-PRCK-SELKRIT       PIC X(15).
             10 PI-DRPA0057-PRCK-GUELT-AB      PIC X(10).
      *
       01 PO-DRPA0057.
          05 PO-DRPA0057-RC                    PIC  X(003).
             88 PO-DRPA0057-RC-OK                      VALUE 'OK '.
             88 PO-DRPA0057-RC-NOT-FOUND               VALUE 'NFD'.
             88 PO-DRPA0057-RC-SEL-KRIT-NOK            VALUE 'SKN'.

          05 PI-DRPA0057-PRCK-DATEN.
             10 PI-DRPA0057-PRCK-GUELT-BIS     PIC X(10).
             10 PI-DRPA0057-KTY-HAFTZEITTYP    PIC S9(4) BINARY.
             10 PI-DRPA0057-KNR-HAFTZEITTYP    PIC S9(4) BINARY.
             10 PI-DRPA0057-PRCK-HAFTZT-PG     PIC S9(4) BINARY.
             10 PI-DRPA0057-PRCK-HAFTZT-VB     PIC S9(4) BINARY.
          05 PI-DRPA0057-PRCK-LAST-UPDATE      PIC X(026).
          05 PI-DRPA0057-PRCK-KENNUNG          PIC X(8).
      *
      ****** ENDE COPY-MEMBER ****************************************

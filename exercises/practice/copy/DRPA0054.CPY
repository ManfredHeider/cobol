      *--------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *     DATUM   :  NOVEMBER 1995
      *     KOPF AUS:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0054.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0054.CPv  $
      *
      *   Rev 2.1   Nov 09 2009 10:18:26   t13ex44
      *APA00820: Pflegedialog Rückstellung/Haftzeiten
      *
      *   REV 2.0   FEB 27 1999 14:27:28   P13SP20
      *NT-UMSTELLUNG
      *
      *   REV 1.1   MAR 06 1997 14:52:26   P13PVCS
      *NEUERSTELLUNG
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0054  FUNKTION INSERT STANDARD-OZP       *
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
      *    FÜR DIE FUNKTION DRPA0054                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<        PRCK TBPA0007                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0054.
          05 PI-DRPA0054-PRCK-KEY.
             10 PI-DRPA0054-PROD-PROD-SCHL        PIC S9(4) USAGE COMP.
             10 PI-DRPA0054-PGEB-GES              PIC S9(4) USAGE COMP.
             10 PI-DRPA0054-PRCK-SELKRIT          PIC X(15).
             10 PI-DRPA0054-PRCK-GUELT-AB         PIC X(10).
          05 PI-DRPA0054-PRCK-DATEN.
             10 PI-DRPA0054-PRCK-GUELT-BIS        PIC X(10).
             10 PI-DRPA0054-KTY-HAFTZEITTYP       PIC S9(4) BINARY.
             10 PI-DRPA0054-KNR-HAFTZEITTYP       PIC S9(4) BINARY.
             10 PI-DRPA0054-PRCK-HAFTZT-PG        PIC S9(4) BINARY.
             10 PI-DRPA0054-PRCK-HAFTZT-VB        PIC S9(4) BINARY.


      *
       01 PO-DRPA0054.
          05 PO-DRPA0054-RC                       PIC  X(003).
             88 PO-DRPA0054-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0054-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER ****************************************

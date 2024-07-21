      *---------------------------------------------------------------*
      *    STANDARD FUER ALLE OBJEKTE UNTER PVCS VM
      *            DATUM      :  06.11.2008
      *            KATEGORIE  :  COPYSTRECKE
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT (C) 2008 ATLAS DIENSTLEISTUNGEN F. V. GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1680.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1680.CPv  $
      *
      *   Rev 2.1   Nov 09 2009 10:19:06   t13ex44
      *APA00820: Pflegedialog Rückstellung/Haftzeiten
      *
      *   REV 2.0   NOV 06 2008 10:16:00   I13AZ23
      *ARCHIVERSTELLUNG

      ****************************************************************
      *    COPY-MEMBER: DRPA1680  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: OBER                                            *
      *    DATUM:    06.11.2008                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00820!06.11.08! 01 ! NEUERSTELLUNG             ! OBER     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1680                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-RÜCKSTELLUNGEN <<<< PRCK TBPA0007             *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1680.
          05 PI-DRPA1680-PRCK-KEY.
             10 PI-DRPA1680-PGEB-GES           PIC S9(4) USAGE COMP.
             10 PI-DRPA1680-PROD-PROD-SCHL     PIC S9(4) USAGE COMP.
             10 PI-DRPA1680-STICHTAG           PIC X(10).
      *
       01 PO-DRPA1680.
          05 PO-DRPA1680-RC                    PIC  X(003).
             88 PO-DRPA1680-RC-OK                      VALUE 'OK '.
             88 PO-DRPA1680-RC-NOT-FOUND               VALUE 'NFD'.

          05 PO-DRPA1680-PRCK-DATEN.
             10 PO-DRPA1680-KNR-HAFTZEITTYP    PIC S9(4) BINARY.
      *---------------------------------------------------------------*

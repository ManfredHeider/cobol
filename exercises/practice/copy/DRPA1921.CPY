      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  14.08.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1921.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1921.CPv  $
      *
      *   Rev 2.1   Aug 26 2019 12:06:12   t13ex67
      *APA02843: WEPA0008
      *
      *   Rev 2.0   Aug 14 2019 13:51:00   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1921  FUNKTION INSERT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH-SCHÄFER                                  *
      *    DATUM:    15.08.2019                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02843!15.08.19! 2.0! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1921                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<      TABELLE PDPE  TBPA0142              *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1921.
           05 PI-DRPA1921-UFKT                    PIC  9(002).
           05 PI-DRPA1921-PDPE-DATEN.
              10 PI-DRPA1921-PGEB-GES             PIC S9(4)   COMP.
              10 PI-DRPA1921-PDPT-ID              PIC S9(15)V COMP-3.
              10 PI-DRPA1921-PDPT-PDPE-NR         PIC S9(4)   COMP.
              10 PI-DRPA1921-PDPE-ST-NR           PIC S9(9)   COMP.
              10 PI-DRPA1921-PDPE-NT-PBTRG-BRWG   PIC S9(9)V9(2) COMP-3.
              10 PI-DRPA1921-PDPE-PROV-RCK-BRWG   PIC S9(9)V9(2) COMP-3.
              10 PI-DRPA1921-PDPE-EINH            PIC S9(6)V9(2) COMP-3.
              10 PI-DRPA1921-PDPE-NT-PBTRG-AZWG   PIC S9(9)V9(2) COMP-3.
              10 PI-DRPA1921-PDPE-PROV-RCK-AZWG   PIC S9(9)V9(2) COMP-3.
              10 PI-DRPA1921-KNR-AUSZ-WAEHRUNG    PIC S9(4)   COMP.
              10 PI-DRPA1921-PDPE-BUCH-RELEVANT   PIC X(1).
              10 PI-DRPA1921-PDPE-UST-PBTRG-BWG   PIC S9(9)V9(2) COMP-3.
              10 PI-DRPA1921-PDPE-UST-RCK-BRWG    PIC S9(9)V9(2) COMP-3.
              10 PI-DRPA1921-PDPE-UST-PBTRG-AWG   PIC S9(9)V9(2) COMP-3.
              10 PI-DRPA1921-PDPE-UST-RCK-AZWG    PIC S9(9)V9(2) COMP-3.
              10 PI-DRPA1921-PDPE-KZ-UST-BER      PIC X(1).
              10 PI-DRPA1921-PDPE-LAST-UPDATE     PIC X(26).
      *
        01 PO-DRPA1921.
           05 PO-DRPA1921-RC           PIC  X(003).
              88 PO-DRPA1921-RC-OK              VALUE 'OK '.
              88 PO-DRPA1921-RC-DUPLICATE-VALUE VALUE 'DUP'.
 

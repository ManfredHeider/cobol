      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  01.07.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0626.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0626.CPv  $
      *
      *   Rev 2.1   Jul 03 2020 07:08:46   t13ex67
      *FPA01775: Abend beim Aufruf einer EVS-Notiz 
      *
      *   Rev 2.0   Jul 01 2020 08:38:42   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0626 FUNKTION ERMITTELN EINER NOTIZ      *
      *                                   ZU EINER ART VON OBJEKT    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    01.07.2020                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA01775!01.07.20!2.01!NEU                        !STORCH    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0626                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: ERMITTELN EINER NOTIZ ZU EINER ART VON OBJEKT   *
      *              BEREITSTELLEN CONTAINERNOTIZ                    *
      ****************************************************************
       01 PI-DRAT0626.
           05  PI-DRAT0626-UFKT                   PIC     99.
               88 PI-DRAT0626-GET-ALL             VALUE   01.
               88 PI-DRAT0626-GET-NEXT            VALUE   03.
      *
      *   KANN BELEGT WERDEN MIT 01=GET ALL, 03=GET-NEXT,
      *   KANN  *NICHT*  BELEGT WERDEN MIT 02=GET FIRST, GET QUIT
      *
       01 PO-DRAT0626.
          05 PO-DRAT0626-RC                        PIC  X(003).
             88 PO-DRAT0626-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0626-RC-LEER                      VALUE 'LER'.
             88 PO-DRAT0626-RC-EOF                       VALUE 'EOF'.
          05 PO-DRAT0626-ERGEBNISBLOCK.
             10 PO-DRAT0626-NTZZ-DATEN.
                15 PO-DRAT0626-NTZZ-ANZ            PIC  S9(04) BINARY.
                15 PO-DRAT0626-NTZZ-TAB.
                   20 PO-DRAT0626-NTZZ-TABELLE   OCCURS 10
                                                 INDEXED DRAT0626-IX1.
                      25 PO-DRAT0626-NTZZ-ZEILE    PIC  X(080).
      *
      ******** ENDE COPY-MEMBER **************************************


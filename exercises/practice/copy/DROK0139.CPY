      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.01.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0139.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0139.CPv  $
      *
      *   Rev 2.1   Mar 27 2014 16:49:02   t13ex27
      *AKI01253: Aktualiserung der Vertragsübersicht
      *
      *   Rev 2.0   Jan 16 2014 15:25:42   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0139                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DROK0139                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01253!16.01.14!2.1 ! neu                       ! IFR      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DROK0139
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DROK0139.
           05  PI-DROK0139-PROGRAMM            PIC  X(008).
           05  PI-DROK0139-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT

       01  PO-DROK0139.
           05  PO-DROK0139-RC                     PIC  X(003).
               88 PO-DROK0139-RC-OK               VALUE 'OK '.
               88 PO-DROK0139-RC-LEER             VALUE 'LER'.
               88 PO-DROK0139-RC-EOF              VALUE 'EOF'.
      *
           05  PO-DROK0139-DATEN.

             10 PO-DROK0139-NAME.
                49 PO-DROK0139-NAME-LEN          PIC S9(4) USAGE COMP.
                49 PO-DROK0139-NAME-TEXT         PIC X(128).
             10 PO-DROK0139-TBNAME.
                49 PO-DROK0139-TBNAME-LEN        PIC S9(4) USAGE COMP.
                49 PO-DROK0139-TBNAME-TEXT       PIC X(128).

      *
      ******* ENDE COPY-MEMBER ***************************************

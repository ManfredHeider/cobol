      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.01.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0068.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DROK0068.CPv  $
      *
      *   Rev 2.2   Apr 17 2008 08:25:58   t13ae49
      *AKL00665: Allfinanz; Spartenermittlung FK, PK
      *
      *   Rev 2.1   Jan 24 2007 08:24:30   t13ex14
      *AKI00556: Datenabgleich HOST-DB <-> Online-DB
      *
      *   Rev 2.0   Jan 19 2007 13:45:18   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0068                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DROK0068                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  23.01.2007/09:21:00
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00556!23.01.07!1.0 ! NEU                       !          *
      *         !        !    !                           !          *
      * AKL00665!03.03.07!1.1 ! AENDERUNG WEGEN HINZUFÜGEN! JWH      *
      *         !        !    ! EINES PK IN TABELLE       !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DROK0068
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DROK0068.
           05  PI-DROK0068-KEY.
             10 PI-DROK0068-SOVPRODUKTID       PIC S9(4) COMP.
030308       10 PI-DROK0068-SOVBEREICH         PIC X(001).
      *
       01  PO-DROK0068.
           05  PO-DROK0068-RC                  PIC  X(003).
               88 PO-DROK0068-RC-OK                        VALUE 'OK '.
               88 PO-DROK0068-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DROK0068-DATEN.
             10 PO-DROK0068-SOVPARTGESNAME     PIC X(25).
             10 PO-DROK0068-SOVPRODUKTBEZ      PIC X(30).
             10 PO-DROK0068-SOVSPARTEVUE       PIC S9(4) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************

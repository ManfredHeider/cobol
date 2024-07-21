      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.08.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2068.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2068.CPv  $
      *
      *   Rev 2.1   Oct 15 2007 18:12:56   t13ex48
      *AKI00677: Änderung der Zuordnung von unbetr. EDI-Dokumenten
      *
      *   Rev 2.0   Aug 27 2007 16:45:26   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2068                                     *
      *    --------------------------------------------------------  *
      *    PI/PO UPRO UPKI0169 (Funktion 2: Ermittlung aktiver Vorg.)*
      *    --------------------------------------------------------  *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00677!23.08.07! 1.0! NEU                       ! HSE      *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2068
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *
      ****************************************************************
      *
       01  PI-DRKI2068.
           05  PI-DRKI2068-DATEN.
             10 PI-DRKI2068-VB-ID              PIC X(13).
             10 PI-DRKI2068-DATUM-ISO          PIC X(10).

      *
       01  PO-DRKI2068.
           05  PO-DRKI2068-RC                 PIC  X(003).
               88 PO-DRKI2068-RC-OK                        VALUE 'OK '.
      * Kein aktiver Vorgesetzter gefunden
               88 PO-DRKI2068-RC-NOT-FOUND                 VALUE 'NFD'.

           05  PO-DRKI2068-VB-NR              PIC  9(07).
           05  PO-DRKI2068-DIR-NR             PIC  9(03).
           05  PO-DRKI2068-VORGES-VB-NR       PIC  9(07).
      *
      ******* ENDE COPY-MEMBER **************************************

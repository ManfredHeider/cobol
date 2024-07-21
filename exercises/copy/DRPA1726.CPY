      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.02.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1726.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1726.CPv  $
      *
      *   Rev 2.1   Mar 07 2011 10:59:22   t13ae37
      *APA01171: I191-ausgelagerte Bewegungungen heranziehen
      *
      *   Rev 2.0   Feb 14 2011 16:39:42   t13ae37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1726                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1726                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BUCH                                             *
      *    DATUM:    14.02.2011                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01171!14.02.11!2.0 ! NEU                       ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1726
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1726.
           05 PI-DRPA1726-PRINTE          PIC S9(015) PACKED-DECIMAL.
           05 PI-DRPA1726-PGEB-GES        PIC S9(004) BINARY.
           05 PI-DRPA1726-PROD-PROD-SCHL  PIC S9(004) BINARY.
           05 PI-DRPA1726-FAELLIG-VON     PIC  X(010).
           05 PI-DRPA1726-GSMT-PROV   PIC S9(009)V9(2) PACKED-DECIMAL.
           05 PI-DRPA1726-ABWEICHUNG  PIC S9(002)V9(2) PACKED-DECIMAL.

       01  PO-DRPA1726.
           05 PO-DRPA1726-RC              PIC  X(003).
              88 PO-DRPA1726-RC-OK                        VALUE 'OK '.
              88 PO-DRPA1726-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05 PO-DRPA1726-SUM-GSMT-PROV PIC S9(009)V9(2) PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************

      *---------------------------------------------------------------*

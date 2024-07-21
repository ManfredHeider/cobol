      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.04.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1886.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1886.CPv  $
      *
      *   Rev 2.3   May 09 2006 10:28:44   t13ex39
      *AKI00215: Umstellung aller Fremdverträge aud EURO
      *
      *   Rev 2.2   May 09 2006 10:13:08   t13ex39
      *AKI00215: Umstellung aller Fremdverträge aud EURO
      *
      *   Rev 2.0   Apr 19 2006 11:18:30   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1886                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1886                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  19.04.2006/11:37:33
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1886
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1886.
           05  PI-DRKI1886-KEY.


             10 PI-DRKI1886-LTSTTNR   PIC S9(4) COMP.
             10 PI-DRKI1886-LTSVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1886-LTSVERTRAGSID   PIC X(13).
             10 PI-DRKI1886-LTSZAHLDYNERHOEH   PIC S9(4) COMP.


           05  PI-DRKI1886-DATEN.


             10 PI-DRKI1886-LTSRATENBEITRAG   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI1886-NI-LTSRATENBEITRAG   PIC S9(4) COMP.
             10 PI-DRKI1886-LTSRATENBEITRWKZ   PIC X(01).
             10 PI-DRKI1886-LTSVERSSUMME   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI1886-NI-LTSVERSSUMME   PIC S9(4) COMP.
             10 PI-DRKI1886-LTSVERSSUMMEWKZ   PIC X(01).


      *
       01  PO-DRKI1886.
           05  PO-DRKI1886-RC                 PIC  X(003).
               88 PO-DRKI1886-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1886-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.04.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2281.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2281.CPv  $
      *
      *   Rev 2.1   06 May 2009 10:49:22   t13ex27
      *AKI00809: Optimierung der Verarbeitung neuer LV-Tarife-OZP
      *
      *   Rev 2.0   07 Apr 2009 17:16:00   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2281                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2281                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *
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
      *    FÜR DIE FUNKTION DRKI2281
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2281.
           05  PI-DRKI2281-KEY.


             10 PI-DRKI2281-LVTLVART   PIC S9(4) COMP.
             10 PI-DRKI2281-LVTTARIF   PIC X(15).

      *
       01  PO-DRKI2281.
           05  PO-DRKI2281-RC                 PIC  X(003).
               88 PO-DRKI2281-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2281-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2281-DATEN.


             10 PO-DRKI2281-LVTPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI2281-LVTSTVD0067-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2281-LVTSTVD0067-KTY   PIC S9(4) COMP.


      *
      ******* ENDE COPY-MEMBER **************************************

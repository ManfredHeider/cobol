      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.05.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0124.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DROK0124.CPv  $
      *
      *   Rev 2.2   Feb 18 2014 16:22:18   t13ex19
      *FKI01448: Anpassung VN-Wechsel für Cloud
      *
      *   Rev 2.1   Jul 20 2012 09:55:30   t13ex27
      *AKI01135: Kundendaten physisch löschen-OZPs und NKDA-Copys
      *
      *   Rev 2.0   May 10 2012 11:34:11 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0124                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DROK0124                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.02.2014 / 16:01:15 / T13EX19
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
      *    FÜR DIE FUNKTION DROK0124
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DROK0124.
           05  PI-DROK0124-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DROK0124-HNAKUNDENID   PIC  X(0013).
END   *


           05  PI-DROK0124-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DROK0124-HNA-KNR   PIC S9(4) COMP.
             10 PI-DROK0124-HNA-KTY   PIC S9(4) COMP.
             10 PI-DROK0124-HNAALTANREDEID   PIC S9(4) COMP.
             10 PI-DROK0124-HNAALTGEBGRUENDDAT   PIC  X(010).
             10 PI-DROK0124-HNAALTNACHNAME   PIC  X(0030).
             10 PI-DROK0124-HNAALTVORNAME   PIC  X(0030).
             10 PI-DROK0124-HNAERSTELLT   PIC  X(026).
             10 PI-DROK0124-HNAERSTELLTVON   PIC  X(0030).
             10 PI-DROK0124-HNANEUANREDEID   PIC S9(4) COMP.
             10 PI-DROK0124-HNANEUGEBGRUENDDAT   PIC  X(010).
             10 PI-DROK0124-HNANEUKUNDENID   PIC  X(0013).
             10 PI-DROK0124-HNANEUNACHNAME   PIC  X(0030).
             10 PI-DROK0124-HNANEUVORNAME   PIC  X(0030).
             10 PI-DROK0124-HNAPARTGESELLSCHID   PIC S9(4) COMP.
             10 PI-DROK0124-HNATICKETID   PIC S9(18)V COMP-3.
             10 PI-DROK0124-HNAVERTRAGSNUMMER   PIC  X(0015).
END   *

      *
       01  PO-DROK0124.
           05  PO-DROK0124-RC                     PIC  X(003).
               88 PO-DROK0124-RC-OK                  VALUE 'OK '.
               88 PO-DROK0124-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

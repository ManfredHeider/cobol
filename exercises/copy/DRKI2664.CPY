      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.10.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2664.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2664.CPv  $
      *
      *   Rev 2.2   Oct 18 2012 10:26:38   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.0   Oct 1 2012 09:25:24 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2664                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2664                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         01.10.2012 / 16:56:54 / T13EX20
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
      *    FÜR DIE FUNKTION DRKI2664
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2664.
           05  PI-DRKI2664-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2664-PHIDATUMHISTORIE   PIC S9(9) COMP.
             10 PI-DRKI2664-PHIPERSONENNR   PIC S9(4) COMP.
             10 PI-DRKI2664-PHITARIFHISTORIE   PIC  X(0012).
             10 PI-DRKI2664-PHIVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2664-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2664-PHIBEITRAG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2664-PHIGESETZZUSCHLG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2664-PHIGUTSCHRIFTBEF   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2664-PHIRISIKOZUSCHLG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2664-PHISELBSTBEHALT   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2664-PHITAGESSATZ   PIC S9(7)V9(2) COMP-3.
END   *

      *
       01  PO-DRKI2664.
           05  PO-DRKI2664-RC                     PIC  X(003).
               88 PO-DRKI2664-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2664-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

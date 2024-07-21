      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.08.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3159.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3159.CPv  $
      *
      *   Rev 2.3   Aug 28 2017 09:58:14   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen
      *
      *   Rev 2.2   Aug 23 2017 05:54:40   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen
      *
      *   Rev 2.1   Aug 08 2017 14:24:30   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen
      *
      *   Rev 2.0   Aug 4 2017 09:24:27 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3159                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3159                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         04.08.2017 / 09:24:27 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3159
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3159.
           05  PI-DRKI3159-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3159-BVHVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI3159.
           05  PO-DRKI3159-RC                     PIC  X(003).
               88 PO-DRKI3159-RC-OK               VALUE 'OK '.
               88 PO-DRKI3159-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI3159-NI.
               10 NI-DRKI3159-TAB.
                  15 DRKI3159-ELE
                     PIC S9(04) COMP OCCURS 9 TIMES.
               10 NI-DRKI3159 REDEFINES NI-DRKI3159-TAB.

ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI3159-NI-BVHAUFSTOCKUNGP   PIC S9(4) COMP.
             15 PO-DRKI3159-NI-BVHEFFEKTIVZINS   PIC S9(4) COMP.
             15 PO-DRKI3159-NI-BVHHOCHKREDITAF   PIC S9(4) COMP.
             15 PO-DRKI3159-NI-BVHLAUFZEIT   PIC S9(4) COMP.
             15 PO-DRKI3159-NI-BVHNOMINALZINS   PIC S9(4) COMP.
             15 PO-DRKI3159-NI-BVHSALDO   PIC S9(4) COMP.
             15 PO-DRKI3159-NI-BVHTILGGRAD   PIC S9(4) COMP.
             15 PO-DRKI3159-NI-BVHVERSPAK-KNR   PIC S9(4) COMP.
             15 PO-DRKI3159-NI-BVHVERSPAK-KTY   PIC S9(4) COMP.
END   *

           05  PO-DRKI3159-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3159-BVHAUFSTOCKUNGP   PIC  X(0001).
             10 PO-DRKI3159-BVHEFFEKTIVZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3159-BVHHOCHKREDITAF   PIC  X(0001).
             10 PO-DRKI3159-BVHLAUFZEIT   PIC S9(4) COMP.
             10 PO-DRKI3159-BVHNOMINALZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3159-BVHSALDO   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3159-BVHTILGGRAD   PIC S9(4) COMP.
             10 PO-DRKI3159-BVHVERSPAK-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI3159-BVHVERSPAK-KTY   PIC S9(4) COMP.
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************




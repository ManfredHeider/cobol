      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.08.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3161.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3161.CPv  $
      *
      *   Rev 2.3   Aug 28 2017 09:58:34   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen
      *
      *   Rev 2.2   Aug 23 2017 05:55:00   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen
      *
      *   Rev 2.1   Aug 08 2017 14:24:56   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen
      *
      *   Rev 2.0   Aug 4 2017 09:24:31 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3161                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3161                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         04.08.2017 / 09:24:31 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3161
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3161.
           05  PI-DRKI3161-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3161-BVHVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI3161-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI3161-NI-BVHAUFSTOCKUNGP PIC S9(4) COMP.
             15 PI-DRKI3161-NI-BVHEFFEKTIVZINS PIC S9(4) COMP.
             15 PI-DRKI3161-NI-BVHHOCHKREDITAF PIC S9(4) COMP.
             15 PI-DRKI3161-NI-BVHLAUFZEIT     PIC S9(4) COMP.
             15 PI-DRKI3161-NI-BVHNOMINALZINS  PIC S9(4) COMP.
             15 PI-DRKI3161-NI-BVHSALDO        PIC S9(4) COMP.
             15 PI-DRKI3161-NI-BVHTILGGRAD     PIC S9(4) COMP.
             15 PI-DRKI3161-NI-BVHVERSPAK-KNR  PIC S9(4) COMP.
             15 PI-DRKI3161-NI-BVHVERSPAK-KTY  PIC S9(4) COMP.
END   *

           05  PI-DRKI3161-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3161-BVHAUFSTOCKUNGP   PIC  X(0001).
             10 PI-DRKI3161-BVHEFFEKTIVZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI3161-BVHHOCHKREDITAF   PIC  X(0001).
             10 PI-DRKI3161-BVHLAUFZEIT   PIC S9(4) COMP.
             10 PI-DRKI3161-BVHNOMINALZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI3161-BVHSALDO   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3161-BVHTILGGRAD   PIC S9(4) COMP.
             10 PI-DRKI3161-BVHVERSPAK-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI3161-BVHVERSPAK-KTY   PIC S9(4) COMP.
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3161-NI1.
           05 PI-DRKI3161-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 9.
       01  PI-DRKI3161-NI7.
           05 PI-DRKI3161-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 9.
      *
       01  PO-DRKI3161.
           05  PO-DRKI3161-RC                     PIC  X(003).
               88 PO-DRKI3161-RC-OK               VALUE 'OK '.
               88 PO-DRKI3161-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************




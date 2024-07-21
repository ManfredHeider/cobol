      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.08.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3160.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3160.CPv  $
      *
      *   Rev 2.3   Aug 28 2017 09:58:24   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen
      *
      *   Rev 2.2   Aug 23 2017 05:54:50   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen
      *
      *   Rev 2.1   Aug 08 2017 14:24:44   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen
      *
      *   Rev 2.0   Aug 4 2017 09:24:29 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3160                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3160                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         04.08.2017 / 09:24:29 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3160
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3160.
           05  PI-DRKI3160-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI3160-BVHVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI3160-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI3160-NI-BVHAUFSTOCKUNGP PIC S9(4) COMP.
             15 PI-DRKI3160-NI-BVHEFFEKTIVZINS PIC S9(4) COMP.
             15 PI-DRKI3160-NI-BVHHOCHKREDITAF PIC S9(4) COMP.
             15 PI-DRKI3160-NI-BVHLAUFZEIT     PIC S9(4) COMP.
             15 PI-DRKI3160-NI-BVHNOMINALZINS  PIC S9(4) COMP.
             15 PI-DRKI3160-NI-BVHSALDO        PIC S9(4) COMP.
             15 PI-DRKI3160-NI-BVHTILGGRAD     PIC S9(4) COMP.
             15 PI-DRKI3160-NI-BVHVERSPAK-KNR  PIC S9(4) COMP.
             15 PI-DRKI3160-NI-BVHVERSPAK-KTY  PIC S9(4) COMP.
END   *

           05  PI-DRKI3160-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3160-BVHAUFSTOCKUNGP   PIC  X(0001).
             10 PI-DRKI3160-BVHEFFEKTIVZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI3160-BVHHOCHKREDITAF   PIC  X(0001).
             10 PI-DRKI3160-BVHLAUFZEIT   PIC S9(4) COMP.
             10 PI-DRKI3160-BVHNOMINALZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI3160-BVHSALDO   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3160-BVHTILGGRAD   PIC S9(4) COMP.
             10 PI-DRKI3160-BVHVERSPAK-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI3160-BVHVERSPAK-KTY   PIC S9(4) COMP.
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3160-NI1.
           05 PI-DRKI3160-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 9.
       01  PI-DRKI3160-NI7.
           05 PI-DRKI3160-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 9.

      *
       01  PO-DRKI3160.
           05  PO-DRKI3160-RC                     PIC  X(003).
               88 PO-DRKI3160-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3160-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************




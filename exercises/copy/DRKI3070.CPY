      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3070.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3070.CPv  $
      *
      *   Rev 2.1   Jun 23 2016 08:09:22   t13ae31
      *AKI01426: G10 SUH VSP Reiseversicherung OZP
      *
      *   Rev 2.0   Apr 21 2016 10:56:55 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3070                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3070                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         06.06.2016 / 12:12:04 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI3070
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3070.
           05  PI-DRKI3070-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3070-RVSVERTRAGSID   PIC  X(0013).
             10 PI-DRKI3070-RVSVETLFDNR   PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3070-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI3070-N-RVSBDNGNG-KNR    PIC S9(4) COMP.
             15 PI-DRKI3070-N-RVSBDNGNG-KTY    PIC S9(4) COMP.
             15 PI-DRKI3070-NI-RVSDIFDECKKZ    PIC S9(4) COMP.
             15 PI-DRKI3070-N-RVSGSLLSCH-KNR   PIC S9(4) COMP.
             15 PI-DRKI3070-N-RVSGSLLSCH-KTY   PIC S9(4) COMP.
             15 PI-DRKI3070-N-RVSNFQLL-KNR     PIC S9(4) COMP.
             15 PI-DRKI3070-N-RVSNFQLL-KTY     PIC S9(4) COMP.
             15 PI-DRKI3070-NI-RVSKUENDIGUNGKZ PIC S9(4) COMP.
             15 PI-DRKI3070-NI-RVSMAILING-KNR  PIC S9(4) COMP.
             15 PI-DRKI3070-NI-RVSMAILING-KTY  PIC S9(4) COMP.
             15 PI-DRKI3070-NI-RVSVERSNUMMER   PIC S9(4) COMP.
             15 PI-DRKI3070-NI-RVSVSUMRGEPCK   PIC S9(4) COMP.
             15 PI-DRKI3070-NI-RVSVSUMRUECK    PIC S9(4) COMP.
             15 PI-DRKI3070-N-RVSVMSTLLNGDT    PIC S9(4) COMP.
             15 PI-DRKI3070-NI-RVSZUSINFO-KNR  PIC S9(4) COMP.
             15 PI-DRKI3070-NI-RVSZUSINFO-KTY  PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3070-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3070-RVSBEDINGUNG-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI3070-RVSBEDINGUNG-KTY   PIC S9(4) COMP.
             10 PI-DRKI3070-RVSDIFDECKKZ   PIC  X(0001).
             10 PI-DRKI3070-RVSEINZVTGHOSTID   PIC  X(0015).
             10 PI-DRKI3070-RVSGESELLSCH-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI3070-RVSGESELLSCH-KTY   PIC S9(4) COMP.
             10 PI-DRKI3070-RVSINFOQUELLE-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI3070-RVSINFOQUELLE-KTY   PIC S9(4) COMP.
             10 PI-DRKI3070-RVSKUENDIGUNGKZ   PIC  X(0001).
             10 PI-DRKI3070-RVSMAILING-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI3070-RVSMAILING-KTY   PIC S9(4) COMP.
             10 PI-DRKI3070-RVSVERSNUMMER   PIC  X(0040).
             10 PI-DRKI3070-RVSVSUMRGEPCK   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI3070-RVSVSUMRUECK   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI3070-RVSVUMSTELLUNGDAT   PIC  X(010).
             10 PI-DRKI3070-RVSZUSINFO-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI3070-RVSZUSINFO-KTY   PIC S9(4) COMP.
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3070-NI1.
           05 PI-DRKI3070-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 16.
       01  PI-DRKI3070-NI7.
           05 PI-DRKI3070-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 16.
      *
       01  PO-DRKI3070.
           05  PO-DRKI3070-RC                     PIC  X(003).
               88 PO-DRKI3070-RC-OK               VALUE 'OK '.
               88 PO-DRKI3070-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************


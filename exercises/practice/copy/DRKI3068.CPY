      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3068.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3068.CPv  $
      *
      *   Rev 2.1   Jun 23 2016 08:09:10   t13ae31
      *AKI01426: G10 SUH VSP Reiseversicherung OZP
      *
      *   Rev 2.0   Apr 21 2016 10:56:50 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3068                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3068                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         06.06.2016 / 12:12:00 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI3068
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3068.
           05  PI-DRKI3068-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3068-RVSVERTRAGSID   PIC  X(0013).
             10 PI-DRKI3068-RVSVETLFDNR   PIC S9(4) COMP.
END   *
 
      *
       01  PO-DRKI3068.
           05  PO-DRKI3068-RC                     PIC  X(003).
               88 PO-DRKI3068-RC-OK               VALUE 'OK '.
               88 PO-DRKI3068-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI3068-NI.
               10 NI-DRKI3068-TAB.
                  15 DRKI3068-ELE
                     PIC S9(04) COMP OCCURS 16 TIMES.
               10 NI-DRKI3068 REDEFINES NI-DRKI3068-TAB.
 
ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI3068-N-RVSBDNGNG-KNR   PIC S9(4) COMP.
             15 PO-DRKI3068-N-RVSBDNGNG-KTY   PIC S9(4) COMP.
             15 PO-DRKI3068-NI-RVSDIFDECKKZ   PIC S9(4) COMP.
             15 PO-DRKI3068-N-RVSGSLLSCH-KNR   PIC S9(4) COMP.
             15 PO-DRKI3068-N-RVSGSLLSCH-KTY   PIC S9(4) COMP.
             15 PO-DRKI3068-N-RVSNFQLL-KNR   PIC S9(4) COMP.
             15 PO-DRKI3068-N-RVSNFQLL-KTY   PIC S9(4) COMP.
             15 PO-DRKI3068-NI-RVSKUENDIGUNGKZ   PIC S9(4) COMP.
             15 PO-DRKI3068-NI-RVSMAILING-KNR   PIC S9(4) COMP.
             15 PO-DRKI3068-NI-RVSMAILING-KTY   PIC S9(4) COMP.
             15 PO-DRKI3068-NI-RVSVERSNUMMER   PIC S9(4) COMP.
             15 PO-DRKI3068-NI-RVSVSUMRGEPCK   PIC S9(4) COMP.
             15 PO-DRKI3068-NI-RVSVSUMRUECK   PIC S9(4) COMP.
             15 PO-DRKI3068-N-RVSVMSTLLNGDT   PIC S9(4) COMP.
             15 PO-DRKI3068-NI-RVSZUSINFO-KNR   PIC S9(4) COMP.
             15 PO-DRKI3068-NI-RVSZUSINFO-KTY   PIC S9(4) COMP.
END   *
 
           05  PO-DRKI3068-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3068-RVSBEDINGUNG-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI3068-RVSBEDINGUNG-KTY   PIC S9(4) COMP.
             10 PO-DRKI3068-RVSDIFDECKKZ   PIC  X(0001).
             10 PO-DRKI3068-RVSEINZVTGHOSTID   PIC  X(0015).
             10 PO-DRKI3068-RVSGESELLSCH-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI3068-RVSGESELLSCH-KTY   PIC S9(4) COMP.
             10 PO-DRKI3068-RVSINFOQUELLE-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI3068-RVSINFOQUELLE-KTY   PIC S9(4) COMP.
             10 PO-DRKI3068-RVSKUENDIGUNGKZ   PIC  X(0001).
             10 PO-DRKI3068-RVSMAILING-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI3068-RVSMAILING-KTY   PIC S9(4) COMP.
             10 PO-DRKI3068-RVSVERSNUMMER   PIC  X(0040).
             10 PO-DRKI3068-RVSVSUMRGEPCK   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI3068-RVSVSUMRUECK   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI3068-RVSVUMSTELLUNGDAT   PIC  X(010).
             10 PO-DRKI3068-RVSZUSINFO-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI3068-RVSZUSINFO-KTY   PIC S9(4) COMP.
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************


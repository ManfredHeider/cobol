      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3073.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3073.CPv  $
      *
      *   Rev 2.1   Jun 23 2016 08:09:40   t13ae31
      *AKI01426: G10 SUH VSP Reiseversicherung OZP
      *
      *   Rev 2.0   Apr 21 2016 10:58:12 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3073                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3073                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         21.04.2016 / 10:58:12 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3073
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3073.
          05 PI-DRKI3073-PROGRAMM                 PIC  X(008).
          05 PI-DRKI3073-UFKT-CODE                PIC  9(002).
 
          05  PI-DRKI3073-KEY.
            10 PI-DRKI3073-VERVERTRAGSNUMMER      PIC  X(015).
            10 PI-DRKI3073-VERPARTGESELLSCHID PIC S9(4) USAGE COMP.
 
      *
       01  PO-DRKI3073.
           05  PO-DRKI3073-RC                     PIC  X(003).
             88 PO-DRKI3073-RC-OK                        VALUE 'OK '.
             88 PO-DRKI3073-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI3073-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI3073-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI3073-DATEN.
 
           10 PO-DRKI3073-RVSVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI3073-RVSVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-RVSEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI3073-RVSBEDINGUNG-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-N-RVSBDNGNG-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-RVSBEDINGUNG-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI3073-N-RVSBDNGNG-KNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-RVSVSUMRGEPCK PIC S9(9)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI3073-NI-RVSVSUMRGEPCK PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-RVSVSUMRUECK PIC S9(9)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI3073-NI-RVSVSUMRUECK PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-RVSMAILING-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-NI-RVSMAILING-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-RVSMAILING-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI3073-NI-RVSMAILING-KNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-RVSZUSINFO-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-NI-RVSZUSINFO-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-RVSZUSINFO-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI3073-NI-RVSZUSINFO-KNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-RVSINFOQUELLE-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-N-RVSNFQLL-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-RVSINFOQUELLE-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI3073-N-RVSNFQLL-KNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-RVSDIFDECKKZ PIC  X(0001)
                                   OCCURS 100.
           10 PO-DRKI3073-RVSGESELLSCH-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-N-RVSGSLLSCH-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-RVSGESELLSCH-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI3073-N-RVSGSLLSCH-KNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-RVSVERSNUMMER PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI3073-NI-RVSVERSNUMMER PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-RVSVUMSTELLUNGDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI3073-N-RVSVMSTLLNGDT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3073-RVSKUENDIGUNGKZ PIC  X(0001)
                                   OCCURS 100.
           10 PO-DRKI3073-LETZTEAENDERUNG PIC  X(0026)
                                   OCCURS 100.
 
      *
      ******* ENDE COPY-MEMBER ***************************************


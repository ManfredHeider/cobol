      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2866.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2866.CPv  $
      *
      *   Rev 2.3   Sep 19 2016 07:41:10   t13ae31
      *AKI01460: SUH SMART-HOME OZP TCKI0410 Hausrat
      *
      *   Rev 2.2   Oct 08 2014 13:40:08   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:01:52   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Apr 15 2014 11:24:53 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2866                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2866                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         05.08.2016 / 13:35:46 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2866
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2866.
          05 PI-DRKI2866-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2866-UFKT-CODE                PIC  9(002).
 
          05  PI-DRKI2866-KEY.
            10 PI-DRKI2866-VERVERTRAGSNUMMER      PIC  X(015).
            10 PI-DRKI2866-VERPARTGESELLSCHID PIC S9(4) USAGE COMP.
 
      *
       01  PO-DRKI2866.
           05  PO-DRKI2866-RC                     PIC  X(003).
             88 PO-DRKI2866-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2866-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2866-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2866-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2866-DATEN.
 
           10 PO-DRKI2866-HRVVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2866-HRVEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVRANSCHRIFTPLZ PIC  X(0006)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVRANSCHRIFTSTR PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVRANSCHRIFTORT PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVRANSCHRIFTLKZ PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2866-HRVBEDINGUNG PIC  X(0016)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVRANSCHRHAUSNR PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVKLAUSELTARIF PIC  X(0080)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVZUSATZINFO PIC  X(0100)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVLEIFREIVONDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2866-N-HRVLFRVNDT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2866-HRVLEIFREIBISDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2866-N-HRVLFRBSDT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2866-HRVSELBSTBKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVSELBSTBPROZ PIC S9(2)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2866-HRVSELBSTBFEST PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2866-HRVSELBSTBTEXT PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVNUTZUNG-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2866-HRVNUTZUNG-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2866-HRVVERSSUMME PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2866-HRVWOHNFLAECHE PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2866-HRVUNTVERSVERZKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVGEBAEUTYP-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2866-HRVGEBAEUTYP-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2866-HRVEIVERHAEL-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2866-HRVEIVERHAEL-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2866-HRVTARIFZONE PIC  X(0004)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVBAUARTKL-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2866-HRVBAUARTKL-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2866-HRVALARMANLAGEKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVFEUERKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVSTURMKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVHAGELKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVEINBRDIEBSTKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVGLASKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVLEITUNGSWASKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVELEMSCHKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVBEZUGSPRAEMIE PIC S9(3)V9(3) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2866-HRVLEISTUNGSTEXT PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVSUMANPKLAUSEL PIC  X(0032)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVPRODUKT PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVMAILINGAKTION PIC  X(0070)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVDIFFDECKKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVUMSTELLUNGDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2866-N-HRVMSTLLNGDT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2866-HRVGESELLSCHAFT PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVVERSNUMMER PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVKUENDIGUNGKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVZIELGRUPPE PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2866-LETZTEAENDERUNG PIC  X(0026)
                                   OCCURS 100.
           10 PO-DRKI2866-HRVSMHPAKET PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2866-NI-HRVSMHPAKET PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2866-HRVSMHDATUM PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2866-NI-HRVSMHDATUM PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2866-HRVSMHTRACKID PIC  X(0060)
                                   OCCURS 100.
           10 PO-DRKI2866-NI-HRVSMHTRACKID PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2866-HRVSMHCONTROLZ PIC  X(0060)
                                   OCCURS 100.
           10 PO-DRKI2866-NI-HRVSMHCONTROLZ PIC S9(4) COMP
                                   OCCURS 100.
 
      *
      ******* ENDE COPY-MEMBER ***************************************


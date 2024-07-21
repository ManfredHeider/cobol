      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.07.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2880.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2880.CPv  $
      *
      *   Rev 2.4   Sep 07 2017 07:40:38   t13ae31
      *AKI01530: G10 SUH SA614 Beruf auf 48 St. OZP
      *
      *   Rev 2.0   Jul 26 2017 10:56:37 T13AE31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2880                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2880                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         26.07.2017 / 10:56:37 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2880
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2880.
          05 PI-DRKI2880-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2880-UFKT-CODE                PIC  9(002).
 
          05  PI-DRKI2880-KEY.
            10 PI-DRKI2880-VERVERTRAGSNUMMER      PIC  X(015).
            10 PI-DRKI2880-VERPARTGESELLSCHID PIC S9(4) USAGE COMP.
      *
       01  PO-DRKI2880.
           05  PO-DRKI2880-RC                     PIC  X(003).
             88 PO-DRKI2880-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2880-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2880-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2880-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2880-DATEN.
 
           10 PO-DRKI2880-UVPVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2880-UVPVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2880-UVPPUFVPLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2880-UVPEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2880-UVPVERSPERSONLFD PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2880-UVPVPNAME PIC  X(0025)
                                   OCCURS 100.
           10 PO-DRKI2880-UVPVPBERUF PIC  X(0021)
                                   OCCURS 100.
           10 PO-DRKI2880-UVPVPGEBURTSDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2880-NI-UVPVPGEBURTSDAT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2880-UVPVPGEFAHRENGRP PIC  X(0001)
                                   OCCURS 100.
           10 PO-DRKI2880-UVPVPBEITRAG PIC S9(7)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2880-UVPTARIFART PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2880-UVPDYNAMIKTEXT PIC  X(0005)
                                   OCCURS 100.
           10 PO-DRKI2880-UVPDYNAMIKPROZ PIC S9(2)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2880-UVPDYNABLJAHR PIC S9(8)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2880-UVPREGULIERUNGKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2880-UVPRUECKGEWSUMKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2880-UVPPRRUECKGEWKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2880-UVPFREIZDECKKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2880-UVPANZVERPERGRP PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2880-UVPVPGESCHL-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2880-UVPVPGESCHL-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2880-UVPPRFREISTKIZKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2880-UVPPRAEMICHEKIZ PIC S9(7)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2880-UVPVPPARTNHOSTID PIC  X(0017)
                                   OCCURS 100.
           10 PO-DRKI2880-UVPDIFFDECKKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2880-UVPZIELGRUPPE PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2880-UVPGESELLSCHAFT PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2880-UVPVERSNUMMER PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2880-UVPUMSTELLUNGDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2880-N-VPMSTLLNGDT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2880-UVPKUENDIGUNGKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2880-LETZTEAENDERUNG PIC  X(0026)
                                   OCCURS 100.
           10 PO-DRKI2880-UVPNACHNAME PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2880-NI-UVPNACHNAME PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2880-UVPVORNAME PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2880-NI-UVPVORNAME PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2880-UVPBERUF-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2880-NI-UVPBERUF-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2880-UVPBERUF-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2880-NI-UVPBERUF-KNR PIC S9(4) COMP
                                   OCCURS 100.
 
      *
      ******* ENDE COPY-MEMBER ***************************************


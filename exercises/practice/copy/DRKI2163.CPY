      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2163.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2163.CPv  $
      *
      *   Rev 2.3   Sep 07 2017 07:40:30   t13ae31
      *AKI01530: G10 SUH SA614 Beruf auf 48 St. OZP
      *
      *   Rev 2.2   Sep 19 2016 07:33:34   t13ae31
      *AKI01458: SUH Namensvorwort bei Versicherten Personen OZP
      *
      *   Rev 2.1   Feb 27 2008 20:05:04   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   28 Sep 2007 17:34:48   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2163                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2163                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         26.07.2017 / 11:03:14 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2163
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2163.
           05  PI-DRKI2163-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2163-UVPPUFVPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2163-UVPVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2163-UVPVETLFDNR   PIC S9(4) COMP.
END   *
 
      *
       01  PO-DRKI2163.
           05  PO-DRKI2163-RC                     PIC  X(003).
               88 PO-DRKI2163-RC-OK               VALUE 'OK '.
               88 PO-DRKI2163-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2163-NI.
               10 NI-DRKI2163-TAB.
                  15 DRKI2163-ELE
                     PIC S9(04) COMP OCCURS 6 TIMES.
               10 NI-DRKI2163 REDEFINES NI-DRKI2163-TAB.
 
ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI2163-NI-UVPBERUF-KNR   PIC S9(4) COMP.
             15 PO-DRKI2163-NI-UVPBERUF-KTY   PIC S9(4) COMP.
             15 PO-DRKI2163-NI-UVPNACHNAME   PIC S9(4) COMP.
             15 PO-DRKI2163-N-VPMSTLLNGDT   PIC S9(4) COMP.
             15 PO-DRKI2163-NI-UVPVORNAME   PIC S9(4) COMP.
             15 PO-DRKI2163-NI-UVPVPGEBURTSDAT   PIC S9(4) COMP.
END   *
 
           05  PO-DRKI2163-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2163-LETZTEAENDERUNG   PIC  X(0026).
             10 PO-DRKI2163-UVPANZVERPERGRP   PIC S9(9) COMP.
             10 PO-DRKI2163-UVPBERUF-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2163-UVPBERUF-KTY   PIC S9(4) COMP.
             10 PO-DRKI2163-UVPDIFFDECKKZ   PIC  X(0002).
             10 PO-DRKI2163-UVPDYNABLJAHR   PIC S9(8)V COMP-3.
             10 PO-DRKI2163-UVPDYNAMIKPROZ   PIC S9(2)V9(2) COMP-3.
             10 PO-DRKI2163-UVPDYNAMIKTEXT   PIC  X(0005).
             10 PO-DRKI2163-UVPEINZVTGHOSTID   PIC  X(0015).
             10 PO-DRKI2163-UVPFREIZDECKKZ   PIC  X(0002).
             10 PO-DRKI2163-UVPGESELLSCHAFT   PIC  X(0050).
             10 PO-DRKI2163-UVPKUENDIGUNGKZ   PIC  X(0002).
             10 PO-DRKI2163-UVPNACHNAME   PIC  X(0040).
             10 PO-DRKI2163-UVPPRAEMICHEKIZ   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI2163-UVPPRFREISTKIZKZ   PIC  X(0002).
             10 PO-DRKI2163-UVPPRRUECKGEWKZ   PIC  X(0002).
             10 PO-DRKI2163-UVPREGULIERUNGKZ   PIC  X(0002).
             10 PO-DRKI2163-UVPRUECKGEWSUMKZ   PIC  X(0002).
             10 PO-DRKI2163-UVPTARIFART   PIC  X(0030).
             10 PO-DRKI2163-UVPUMSTELLUNGDAT   PIC  X(010).
             10 PO-DRKI2163-UVPVERSNUMMER   PIC  X(0040).
             10 PO-DRKI2163-UVPVERSPERSONLFD   PIC S9(4) COMP.
             10 PO-DRKI2163-UVPVORNAME   PIC  X(0030).
             10 PO-DRKI2163-UVPVPBEITRAG   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI2163-UVPVPBERUF   PIC  X(0021).
             10 PO-DRKI2163-UVPVPGEBURTSDAT   PIC  X(010).
             10 PO-DRKI2163-UVPVPGEFAHRENGRP   PIC  X(0001).
             10 PO-DRKI2163-UVPVPGESCHL-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2163-UVPVPGESCHL-KTY   PIC S9(4) COMP.
             10 PO-DRKI2163-UVPVPNAME   PIC  X(0025).
             10 PO-DRKI2163-UVPVPPARTNHOSTID   PIC  X(0017).
             10 PO-DRKI2163-UVPZIELGRUPPE   PIC  X(0030).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************


      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2161.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2161.CPv  $
      *
      *   Rev 2.3   Sep 07 2017 07:40:24   t13ae31
      *AKI01530: G10 SUH SA614 Beruf auf 48 St. OZP
      *
      *   Rev 2.2   Sep 19 2016 07:33:02   t13ae31
      *AKI01458: SUH Namensvorwort bei Versicherten Personen OZP
      *
      *   Rev 2.1   Feb 27 2008 20:04:50   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   28 Sep 2007 17:28:16   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2161                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2161                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         26.07.2017 / 11:03:10 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2161
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2161.
           05  PI-DRKI2161-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2161-UVPPUFVPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2161-UVPVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2161-UVPVETLFDNR   PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2161-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2161-NI-UVPANZVERPERGRP  PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPBERUF-KNR     PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPBERUF-KTY     PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPDIFFDECKKZ    PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPDYNABLJAHR    PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPDYNAMIKPROZ   PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPDYNAMIKTEXT   PIC S9(4) COMP.
             15 PI-DRKI2161-N-VPNZVTGHSTD       PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPFREIZDECKKZ   PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPGESELLSCHAFT  PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPKUENDIGUNGKZ  PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPNACHNAME      PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPPRAEMICHEKIZ  PIC S9(4) COMP.
             15 PI-DRKI2161-N-VPPRFRSTKZKZ      PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPPRRUECKGEWKZ  PIC S9(4) COMP.
             15 PI-DRKI2161-N-VPRGLRNGKZ        PIC S9(4) COMP.
             15 PI-DRKI2161-N-VPRCKGWSMKZ       PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPTARIFART      PIC S9(4) COMP.
             15 PI-DRKI2161-N-VPMSTLLNGDT       PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPVERSNUMMER    PIC S9(4) COMP.
             15 PI-DRKI2161-N-VPVRSPRSNLFD      PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPVORNAME       PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPVPBEITRAG     PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPVPBERUF       PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPVPGEBURTSDAT  PIC S9(4) COMP.
             15 PI-DRKI2161-N-VPVPGFHRNGRP      PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPVPGESCHL-KNR  PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPVPGESCHL-KTY  PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPVPNAME        PIC S9(4) COMP.
             15 PI-DRKI2161-N-VPVPPRTNHSTD      PIC S9(4) COMP.
             15 PI-DRKI2161-NI-UVPZIELGRUPPE    PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2161-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2161-UVPANZVERPERGRP   PIC S9(9) COMP.
             10 PI-DRKI2161-UVPBERUF-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2161-UVPBERUF-KTY   PIC S9(4) COMP.
             10 PI-DRKI2161-UVPDIFFDECKKZ   PIC  X(0002).
             10 PI-DRKI2161-UVPDYNABLJAHR   PIC S9(8)V COMP-3.
             10 PI-DRKI2161-UVPDYNAMIKPROZ   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI2161-UVPDYNAMIKTEXT   PIC  X(0005).
             10 PI-DRKI2161-UVPEINZVTGHOSTID   PIC  X(0015).
             10 PI-DRKI2161-UVPFREIZDECKKZ   PIC  X(0002).
             10 PI-DRKI2161-UVPGESELLSCHAFT   PIC  X(0050).
             10 PI-DRKI2161-UVPKUENDIGUNGKZ   PIC  X(0002).
             10 PI-DRKI2161-UVPNACHNAME   PIC  X(0040).
             10 PI-DRKI2161-UVPPRAEMICHEKIZ   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2161-UVPPRFREISTKIZKZ   PIC  X(0002).
             10 PI-DRKI2161-UVPPRRUECKGEWKZ   PIC  X(0002).
             10 PI-DRKI2161-UVPREGULIERUNGKZ   PIC  X(0002).
             10 PI-DRKI2161-UVPRUECKGEWSUMKZ   PIC  X(0002).
             10 PI-DRKI2161-UVPTARIFART   PIC  X(0030).
             10 PI-DRKI2161-UVPUMSTELLUNGDAT   PIC  X(010).
             10 PI-DRKI2161-UVPVERSNUMMER   PIC  X(0040).
             10 PI-DRKI2161-UVPVERSPERSONLFD   PIC S9(4) COMP.
             10 PI-DRKI2161-UVPVORNAME   PIC  X(0030).
             10 PI-DRKI2161-UVPVPBEITRAG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2161-UVPVPBERUF   PIC  X(0021).
             10 PI-DRKI2161-UVPVPGEBURTSDAT   PIC  X(010).
             10 PI-DRKI2161-UVPVPGEFAHRENGRP   PIC  X(0001).
             10 PI-DRKI2161-UVPVPGESCHL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2161-UVPVPGESCHL-KTY   PIC S9(4) COMP.
             10 PI-DRKI2161-UVPVPNAME   PIC  X(0025).
             10 PI-DRKI2161-UVPVPPARTNHOSTID   PIC  X(0017).
             10 PI-DRKI2161-UVPZIELGRUPPE   PIC  X(0030).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2161-NI1.
           05 PI-DRKI2161-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 31.
       01  PI-DRKI2161-NI7.
           05 PI-DRKI2161-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 31.
      *
       01  PO-DRKI2161.
           05  PO-DRKI2161-RC                     PIC  X(003).
               88 PO-DRKI2161-RC-OK               VALUE 'OK '.
               88 PO-DRKI2161-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************


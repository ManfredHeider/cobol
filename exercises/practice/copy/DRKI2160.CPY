      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2160.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2160.CPv  $
      *
      *   Rev 2.3   Sep 07 2017 07:40:16   t13ae31
      *AKI01530: G10 SUH SA614 Beruf auf 48 St. OZP
      *
      *   Rev 2.2   Sep 19 2016 07:32:56   t13ae31
      *AKI01458: SUH Namensvorwort bei Versicherten Personen OZP
      *
      *   Rev 2.1   Feb 27 2008 20:04:42   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   28 Sep 2007 17:25:04   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2160                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2160                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         26.07.2017 / 11:03:07 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2160
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2160.
           05  PI-DRKI2160-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2160-UVPPUFVPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2160-UVPVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2160-UVPVETLFDNR   PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2160-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2160-NI-UVPBERUF-KNR     PIC S9(4) COMP.
             15 PI-DRKI2160-NI-UVPBERUF-KTY     PIC S9(4) COMP.
             15 PI-DRKI2160-NI-UVPNACHNAME      PIC S9(4) COMP.
             15 PI-DRKI2160-N-VPMSTLLNGDT       PIC S9(4) COMP.
             15 PI-DRKI2160-NI-UVPVORNAME       PIC S9(4) COMP.
             15 PI-DRKI2160-NI-UVPVPGEBURTSDAT  PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2160-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2160-UVPANZVERPERGRP   PIC S9(9) COMP.
             10 PI-DRKI2160-UVPBERUF-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2160-UVPBERUF-KTY   PIC S9(4) COMP.
             10 PI-DRKI2160-UVPDIFFDECKKZ   PIC  X(0002).
             10 PI-DRKI2160-UVPDYNABLJAHR   PIC S9(8)V COMP-3.
             10 PI-DRKI2160-UVPDYNAMIKPROZ   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI2160-UVPDYNAMIKTEXT   PIC  X(0005).
             10 PI-DRKI2160-UVPEINZVTGHOSTID   PIC  X(0015).
             10 PI-DRKI2160-UVPFREIZDECKKZ   PIC  X(0002).
             10 PI-DRKI2160-UVPGESELLSCHAFT   PIC  X(0050).
             10 PI-DRKI2160-UVPKUENDIGUNGKZ   PIC  X(0002).
             10 PI-DRKI2160-UVPNACHNAME   PIC  X(0040).
             10 PI-DRKI2160-UVPPRAEMICHEKIZ   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2160-UVPPRFREISTKIZKZ   PIC  X(0002).
             10 PI-DRKI2160-UVPPRRUECKGEWKZ   PIC  X(0002).
             10 PI-DRKI2160-UVPREGULIERUNGKZ   PIC  X(0002).
             10 PI-DRKI2160-UVPRUECKGEWSUMKZ   PIC  X(0002).
             10 PI-DRKI2160-UVPTARIFART   PIC  X(0030).
             10 PI-DRKI2160-UVPUMSTELLUNGDAT   PIC  X(010).
             10 PI-DRKI2160-UVPVERSNUMMER   PIC  X(0040).
             10 PI-DRKI2160-UVPVERSPERSONLFD   PIC S9(4) COMP.
             10 PI-DRKI2160-UVPVORNAME   PIC  X(0030).
             10 PI-DRKI2160-UVPVPBEITRAG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2160-UVPVPBERUF   PIC  X(0021).
             10 PI-DRKI2160-UVPVPGEBURTSDAT   PIC  X(010).
             10 PI-DRKI2160-UVPVPGEFAHRENGRP   PIC  X(0001).
             10 PI-DRKI2160-UVPVPGESCHL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2160-UVPVPNAME   PIC  X(0025).
             10 PI-DRKI2160-UVPVPPARTNHOSTID   PIC  X(0017).
             10 PI-DRKI2160-UVPZIELGRUPPE   PIC  X(0030).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2160-NI1.
           05 PI-DRKI2160-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 6.
       01  PI-DRKI2160-NI7.
           05 PI-DRKI2160-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 6.
 
      *
       01  PO-DRKI2160.
           05  PO-DRKI2160-RC                     PIC  X(003).
               88 PO-DRKI2160-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2160-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************


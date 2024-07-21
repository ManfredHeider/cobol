      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2122.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2122.CPv  $
      *
      *   Rev 2.2   Sep 19 2016 07:41:44   t13ae31
      *AKI01460: SUH SMART-HOME OZP TCKI0410 Hausrat
      *
      *   Rev 2.1   Feb 27 2008 20:02:06   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   28 Sep 2007 12:26:20   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2122                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2122                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.08.2016 / 10:23:39 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2122
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2122.
           05  PI-DRKI2122-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2122-HRVVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2122-HRVVETLFDNR   PIC S9(4) COMP.
END   *
 
      *
       01  PO-DRKI2122.
           05  PO-DRKI2122-RC                     PIC  X(003).
               88 PO-DRKI2122-RC-OK               VALUE 'OK '.
               88 PO-DRKI2122-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2122-NI.
               10 NI-DRKI2122-TAB.
                  15 DRKI2122-ELE
                     PIC S9(04) COMP OCCURS 7 TIMES.
               10 NI-DRKI2122 REDEFINES NI-DRKI2122-TAB.
 
ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI2122-N-HRVLFRBSDT   PIC S9(4) COMP.
             15 PO-DRKI2122-N-HRVLFRVNDT   PIC S9(4) COMP.
             15 PO-DRKI2122-NI-HRVSMHCONTROLZ   PIC S9(4) COMP.
             15 PO-DRKI2122-NI-HRVSMHDATUM   PIC S9(4) COMP.
             15 PO-DRKI2122-NI-HRVSMHPAKET   PIC S9(4) COMP.
             15 PO-DRKI2122-NI-HRVSMHTRACKID   PIC S9(4) COMP.
             15 PO-DRKI2122-N-HRVMSTLLNGDT   PIC S9(4) COMP.
END   *
 
           05  PO-DRKI2122-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2122-HRVALARMANLAGEKZ   PIC  X(0002).
             10 PO-DRKI2122-HRVBAUARTKL-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2122-HRVBAUARTKL-KTY   PIC S9(4) COMP.
             10 PO-DRKI2122-HRVBEDINGUNG   PIC  X(0016).
             10 PO-DRKI2122-HRVBEZUGSPRAEMIE   PIC S9(3)V9(3) COMP-3.
             10 PO-DRKI2122-HRVDIFFDECKKZ   PIC  X(0002).
             10 PO-DRKI2122-HRVEINBRDIEBSTKZ   PIC  X(0002).
             10 PO-DRKI2122-HRVEINZVTGHOSTID   PIC  X(0015).
             10 PO-DRKI2122-HRVEIVERHAEL-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2122-HRVEIVERHAEL-KTY   PIC S9(4) COMP.
             10 PO-DRKI2122-HRVELEMSCHKZ   PIC  X(0002).
             10 PO-DRKI2122-HRVFEUERKZ   PIC  X(0002).
             10 PO-DRKI2122-HRVGEBAEUTYP-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2122-HRVGEBAEUTYP-KTY   PIC S9(4) COMP.
             10 PO-DRKI2122-HRVGESELLSCHAFT   PIC  X(0050).
             10 PO-DRKI2122-HRVGLASKZ   PIC  X(0002).
             10 PO-DRKI2122-HRVHAGELKZ   PIC  X(0002).
             10 PO-DRKI2122-HRVKLAUSELTARIF   PIC  X(0080).
             10 PO-DRKI2122-HRVKUENDIGUNGKZ   PIC  X(0002).
             10 PO-DRKI2122-HRVLEIFREIBISDAT   PIC  X(010).
             10 PO-DRKI2122-HRVLEIFREIVONDAT   PIC  X(010).
             10 PO-DRKI2122-HRVLEISTUNGSTEXT   PIC  X(0040).
             10 PO-DRKI2122-HRVLEITUNGSWASKZ   PIC  X(0002).
             10 PO-DRKI2122-HRVMAILINGAKTION   PIC  X(0070).
             10 PO-DRKI2122-HRVNUTZUNG-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2122-HRVNUTZUNG-KTY   PIC S9(4) COMP.
             10 PO-DRKI2122-HRVPRODUKT   PIC  X(0030).
             10 PO-DRKI2122-HRVRANSCHRHAUSNR   PIC  X(0015).
             10 PO-DRKI2122-HRVRANSCHRIFTLKZ   PIC S9(9) COMP.
             10 PO-DRKI2122-HRVRANSCHRIFTORT   PIC  X(0030).
             10 PO-DRKI2122-HRVRANSCHRIFTPLZ   PIC  X(0006).
             10 PO-DRKI2122-HRVRANSCHRIFTSTR   PIC  X(0040).
             10 PO-DRKI2122-HRVSELBSTBFEST   PIC S9(9) COMP.
             10 PO-DRKI2122-HRVSELBSTBKZ   PIC  X(0002).
             10 PO-DRKI2122-HRVSELBSTBPROZ   PIC S9(2)V9(2) COMP-3.
             10 PO-DRKI2122-HRVSELBSTBTEXT   PIC  X(0050).
             10 PO-DRKI2122-HRVSMHCONTROLZ   PIC  X(0060).
             10 PO-DRKI2122-HRVSMHDATUM   PIC  X(010).
             10 PO-DRKI2122-HRVSMHPAKET   PIC  X(0002).
             10 PO-DRKI2122-HRVSMHTRACKID   PIC  X(0060).
             10 PO-DRKI2122-HRVSTURMKZ   PIC  X(0002).
             10 PO-DRKI2122-HRVSUMANPKLAUSEL   PIC  X(0032).
             10 PO-DRKI2122-HRVTARIFZONE   PIC  X(0004).
             10 PO-DRKI2122-HRVUMSTELLUNGDAT   PIC  X(010).
             10 PO-DRKI2122-HRVUNTVERSVERZKZ   PIC  X(0002).
             10 PO-DRKI2122-HRVVERSNUMMER   PIC  X(0040).
             10 PO-DRKI2122-HRVVERSSUMME   PIC S9(9) COMP.
             10 PO-DRKI2122-HRVWOHNFLAECHE   PIC S9(9) COMP.
             10 PO-DRKI2122-HRVZIELGRUPPE   PIC  X(0030).
             10 PO-DRKI2122-HRVZUSATZINFO   PIC  X(0100).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************


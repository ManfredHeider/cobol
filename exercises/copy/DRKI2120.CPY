      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2120.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2120.CPv  $
      *
      *   Rev 2.3   Sep 21 2016 07:39:06   t13ae31
      *AKI01460: SUH SMART-HOME Nachtrag
      *
      *   Rev 2.2   Sep 19 2016 07:41:32   t13ae31
      *AKI01460: SUH SMART-HOME OZP TCKI0410 Hausrat
      *
      *   Rev 2.1   Feb 27 2008 20:01:52   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   28 Sep 2007 12:20:32   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2120                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2120                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.09.2016 / 10:20:09 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2120
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2120.
           05  PI-DRKI2120-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2120-HRVVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2120-HRVVETLFDNR   PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2120-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2120-N-HRVLRMNLGKZ      PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVBAUARTKL-KNR PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVBAUARTKL-KTY PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVBEDINGUNG    PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVBZGSPRM       PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVDIFFDECKKZ   PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVNBRDBSTKZ     PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVVRHL-KNR      PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVVRHL-KTY      PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVELEMSCHKZ    PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVFEUERKZ      PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVGBTYP-KNR     PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVGBTYP-KTY     PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVGESELLSCHAFT PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVGLASKZ       PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVHAGELKZ      PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVKLAUSELTARIF PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVKUENDIGUNGKZ PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVLFRBSDT       PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVLFRVNDT       PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVLSTNGSTXT     PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVLTNGSWSKZ     PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVMLNGKTN       PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVNUTZUNG-KNR  PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVNUTZUNG-KTY  PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVPRODUKT      PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVRNSCHRHSNR    PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVRNSCHRFTLKZ   PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVRNSCHRFTRT    PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVRNSCHRFTPLZ   PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVRNSCHRFTSTR   PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVSELBSTBFEST  PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVSELBSTBKZ    PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVSELBSTBPROZ  PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVSELBSTBTEXT  PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVSMHCONTROLZ  PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVSMHDATUM     PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVSMHPAKET     PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVSMHTRACKID   PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVSTURMKZ      PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVSMNPKLSL      PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVTARIFZONE    PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVMSTLLNGDT     PIC S9(4) COMP.
             15 PI-DRKI2120-N-HRVNTVRSVRZKZ    PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVVERSNUMMER   PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVVERSSUMME    PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVWOHNFLAECHE  PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVZIELGRUPPE   PIC S9(4) COMP.
             15 PI-DRKI2120-NI-HRVZUSATZINFO   PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2120-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2120-HRVALARMANLAGEKZ   PIC  X(0002).
             10 PI-DRKI2120-HRVBAUARTKL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2120-HRVBAUARTKL-KTY   PIC S9(4) COMP.
             10 PI-DRKI2120-HRVBEDINGUNG   PIC  X(0016).
             10 PI-DRKI2120-HRVBEZUGSPRAEMIE   PIC S9(3)V9(3) COMP-3.
             10 PI-DRKI2120-HRVDIFFDECKKZ   PIC  X(0002).
             10 PI-DRKI2120-HRVEINBRDIEBSTKZ   PIC  X(0002).
             10 PI-DRKI2120-HRVEINZVTGHOSTID   PIC  X(0015).
             10 PI-DRKI2120-HRVEIVERHAEL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2120-HRVEIVERHAEL-KTY   PIC S9(4) COMP.
             10 PI-DRKI2120-HRVELEMSCHKZ   PIC  X(0002).
             10 PI-DRKI2120-HRVFEUERKZ   PIC  X(0002).
             10 PI-DRKI2120-HRVGEBAEUTYP-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2120-HRVGEBAEUTYP-KTY   PIC S9(4) COMP.
             10 PI-DRKI2120-HRVGESELLSCHAFT   PIC  X(0050).
             10 PI-DRKI2120-HRVGLASKZ   PIC  X(0002).
             10 PI-DRKI2120-HRVHAGELKZ   PIC  X(0002).
             10 PI-DRKI2120-HRVKLAUSELTARIF   PIC  X(0080).
             10 PI-DRKI2120-HRVKUENDIGUNGKZ   PIC  X(0002).
             10 PI-DRKI2120-HRVLEIFREIBISDAT   PIC  X(010).
             10 PI-DRKI2120-HRVLEIFREIVONDAT   PIC  X(010).
             10 PI-DRKI2120-HRVLEISTUNGSTEXT   PIC  X(0040).
             10 PI-DRKI2120-HRVLEITUNGSWASKZ   PIC  X(0002).
             10 PI-DRKI2120-HRVMAILINGAKTION   PIC  X(0070).
             10 PI-DRKI2120-HRVNUTZUNG-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2120-HRVNUTZUNG-KTY   PIC S9(4) COMP.
             10 PI-DRKI2120-HRVPRODUKT   PIC  X(0030).
             10 PI-DRKI2120-HRVRANSCHRHAUSNR   PIC  X(0015).
             10 PI-DRKI2120-HRVRANSCHRIFTLKZ   PIC S9(9) COMP.
             10 PI-DRKI2120-HRVRANSCHRIFTORT   PIC  X(0030).
             10 PI-DRKI2120-HRVRANSCHRIFTPLZ   PIC  X(0006).
             10 PI-DRKI2120-HRVRANSCHRIFTSTR   PIC  X(0040).
             10 PI-DRKI2120-HRVSELBSTBFEST   PIC S9(9) COMP.
             10 PI-DRKI2120-HRVSELBSTBKZ   PIC  X(0002).
             10 PI-DRKI2120-HRVSELBSTBPROZ   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI2120-HRVSELBSTBTEXT   PIC  X(0050).
             10 PI-DRKI2120-HRVSMHCONTROLZ   PIC  X(0060).
             10 PI-DRKI2120-HRVSMHDATUM   PIC  X(010).
             10 PI-DRKI2120-HRVSMHPAKET   PIC  X(0002).
             10 PI-DRKI2120-HRVSMHTRACKID   PIC  X(0060).
             10 PI-DRKI2120-HRVSTURMKZ   PIC  X(0002).
             10 PI-DRKI2120-HRVSUMANPKLAUSEL   PIC  X(0032).
             10 PI-DRKI2120-HRVTARIFZONE   PIC  X(0004).
             10 PI-DRKI2120-HRVUMSTELLUNGDAT   PIC  X(010).
             10 PI-DRKI2120-HRVUNTVERSVERZKZ   PIC  X(0002).
             10 PI-DRKI2120-HRVVERSNUMMER   PIC  X(0040).
             10 PI-DRKI2120-HRVVERSSUMME   PIC S9(9) COMP.
             10 PI-DRKI2120-HRVWOHNFLAECHE   PIC S9(9) COMP.
             10 PI-DRKI2120-HRVZIELGRUPPE   PIC  X(0030).
             10 PI-DRKI2120-HRVZUSATZINFO   PIC  X(0100).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2120-NI1.
           05 PI-DRKI2120-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 49.
       01  PI-DRKI2120-NI7.
           05 PI-DRKI2120-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 49.
      *
       01  PO-DRKI2120.
           05  PO-DRKI2120-RC                     PIC  X(003).
               88 PO-DRKI2120-RC-OK               VALUE 'OK '.
               88 PO-DRKI2120-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************


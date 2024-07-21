      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2119.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2119.CPv  $
      *
      *   Rev 2.2   Sep 19 2016 07:41:26   t13ae31
      *AKI01460: SUH SMART-HOME OZP TCKI0410 Hausrat
      *
      *   Rev 2.1   Feb 27 2008 20:01:44   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   28 Sep 2007 12:17:36   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2119                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2119                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.08.2016 / 10:23:31 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2119
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2119.
           05  PI-DRKI2119-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2119-HRVVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2119-HRVVETLFDNR   PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2119-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2119-N-HRVLFRBSDT       PIC S9(4) COMP.
             15 PI-DRKI2119-N-HRVLFRVNDT       PIC S9(4) COMP.
             15 PI-DRKI2119-NI-HRVSMHCONTROLZ  PIC S9(4) COMP.
             15 PI-DRKI2119-NI-HRVSMHDATUM     PIC S9(4) COMP.
             15 PI-DRKI2119-NI-HRVSMHPAKET     PIC S9(4) COMP.
             15 PI-DRKI2119-NI-HRVSMHTRACKID   PIC S9(4) COMP.
             15 PI-DRKI2119-N-HRVMSTLLNGDT     PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2119-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2119-HRVALARMANLAGEKZ   PIC  X(0002).
             10 PI-DRKI2119-HRVBAUARTKL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2119-HRVBAUARTKL-KTY   PIC S9(4) COMP.
             10 PI-DRKI2119-HRVBEDINGUNG   PIC  X(0016).
             10 PI-DRKI2119-HRVBEZUGSPRAEMIE   PIC S9(3)V9(3) COMP-3.
             10 PI-DRKI2119-HRVDIFFDECKKZ   PIC  X(0002).
             10 PI-DRKI2119-HRVEINBRDIEBSTKZ   PIC  X(0002).
             10 PI-DRKI2119-HRVEINZVTGHOSTID   PIC  X(0015).
             10 PI-DRKI2119-HRVEIVERHAEL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2119-HRVEIVERHAEL-KTY   PIC S9(4) COMP.
             10 PI-DRKI2119-HRVELEMSCHKZ   PIC  X(0002).
             10 PI-DRKI2119-HRVFEUERKZ   PIC  X(0002).
             10 PI-DRKI2119-HRVGEBAEUTYP-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2119-HRVGEBAEUTYP-KTY   PIC S9(4) COMP.
             10 PI-DRKI2119-HRVGESELLSCHAFT   PIC  X(0050).
             10 PI-DRKI2119-HRVGLASKZ   PIC  X(0002).
             10 PI-DRKI2119-HRVHAGELKZ   PIC  X(0002).
             10 PI-DRKI2119-HRVKLAUSELTARIF   PIC  X(0080).
             10 PI-DRKI2119-HRVKUENDIGUNGKZ   PIC  X(0002).
             10 PI-DRKI2119-HRVLEIFREIBISDAT   PIC  X(010).
             10 PI-DRKI2119-HRVLEIFREIVONDAT   PIC  X(010).
             10 PI-DRKI2119-HRVLEISTUNGSTEXT   PIC  X(0040).
             10 PI-DRKI2119-HRVLEITUNGSWASKZ   PIC  X(0002).
             10 PI-DRKI2119-HRVMAILINGAKTION   PIC  X(0070).
             10 PI-DRKI2119-HRVNUTZUNG-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2119-HRVNUTZUNG-KTY   PIC S9(4) COMP.
             10 PI-DRKI2119-HRVPRODUKT   PIC  X(0030).
             10 PI-DRKI2119-HRVRANSCHRHAUSNR   PIC  X(0015).
             10 PI-DRKI2119-HRVRANSCHRIFTLKZ   PIC S9(9) COMP.
             10 PI-DRKI2119-HRVRANSCHRIFTORT   PIC  X(0030).
             10 PI-DRKI2119-HRVRANSCHRIFTPLZ   PIC  X(0006).
             10 PI-DRKI2119-HRVRANSCHRIFTSTR   PIC  X(0040).
             10 PI-DRKI2119-HRVSELBSTBFEST   PIC S9(9) COMP.
             10 PI-DRKI2119-HRVSELBSTBKZ   PIC  X(0002).
             10 PI-DRKI2119-HRVSELBSTBPROZ   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI2119-HRVSELBSTBTEXT   PIC  X(0050).
             10 PI-DRKI2119-HRVSMHCONTROLZ   PIC  X(0060).
             10 PI-DRKI2119-HRVSMHDATUM   PIC  X(010).
             10 PI-DRKI2119-HRVSMHPAKET   PIC  X(0002).
             10 PI-DRKI2119-HRVSMHTRACKID   PIC  X(0060).
             10 PI-DRKI2119-HRVSTURMKZ   PIC  X(0002).
             10 PI-DRKI2119-HRVSUMANPKLAUSEL   PIC  X(0032).
             10 PI-DRKI2119-HRVTARIFZONE   PIC  X(0004).
             10 PI-DRKI2119-HRVUMSTELLUNGDAT   PIC  X(010).
             10 PI-DRKI2119-HRVUNTVERSVERZKZ   PIC  X(0002).
             10 PI-DRKI2119-HRVVERSNUMMER   PIC  X(0040).
             10 PI-DRKI2119-HRVVERSSUMME   PIC S9(9) COMP.
             10 PI-DRKI2119-HRVWOHNFLAECHE   PIC S9(9) COMP.
             10 PI-DRKI2119-HRVZIELGRUPPE   PIC  X(0030).
             10 PI-DRKI2119-HRVZUSATZINFO   PIC  X(0100).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2119-NI1.
           05 PI-DRKI2119-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 7.
       01  PI-DRKI2119-NI7.
           05 PI-DRKI2119-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 7.
 
      *
       01  PO-DRKI2119.
           05  PO-DRKI2119-RC                     PIC  X(003).
               88 PO-DRKI2119-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2119-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************


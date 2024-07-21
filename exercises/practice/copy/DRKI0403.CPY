      *---------------------------------------------------------------*
      *    Standard fˆr alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0403.CPY  $
      *    $Revision:   2.13  $
      *
      *    $Log:   L:/mf/CPY/DRKI0403.CPv  $
      *
      *   Rev 2.13   Mar 30 2020 08:30:02   t13ae31
      *AKI01766: G81 Advocard neue Felder u.a.
      *
      *   Rev 2.12   Dec 09 2019 13:50:50   t13ae31
      *AKI01734: G81 Advocard Nettopreise Stornodatum UPKI0050 OZP
      *
      *   Rev 2.11   Jan 11 2016 14:56:02   t13ex78
      *AKI01404: AdvoCard - Kombirabatt und Kundenbonus trennen - Z
      *TKI0353
      *
      *   Rev 2.10   Aug 28 2013 13:18:44   t13ex19
      *AKI01249: AdvoCard - Sofortrabatt
      *
      *   Rev 2.9   Jul 21 2009 13:32:52   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 1.0   14 Jul 1998 17:05:10   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0403                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0403                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         26.04.2021 / 13:34:18 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI0403
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0403.
           05  PI-DRKI0403-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI0403-SATVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0403-SATVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI0403-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI0403-NI-SATANNAHMEDAT    PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATBESTMANAGKZ   PIC S9(4) COMP.
             15 PI-DRKI0403-N-STDRNCHLSS        PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATDIFFABLAUF    PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATDIFFGES       PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATDIFFVNR       PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATEEGFKOSTENG   PIC S9(4) COMP.
             15 PI-DRKI0403-N-STGFKSTNGWKZ      PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATEGKUENSTGS    PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATEGKUENSTGSKZ  PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATEGPRIVATGH    PIC S9(4) COMP.
             15 PI-DRKI0403-N-STGPRVTGHWKZ      PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATGLASBERECHID  PIC S9(4) COMP.
             15 PI-DRKI0403-N-STGLSBRCHWRT      PIC S9(4) COMP.
             15 PI-DRKI0403-N-STGLSBRWRTWKZ     PIC S9(4) COMP.
             15 PI-DRKI0403-N-STHNWFSLBSTB      PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATJAHRBEITRWKZ  PIC S9(4) COMP.
             15 PI-DRKI0403-N-STJHRSBTRG        PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATJHRBNETTOWKZ  PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATJHRBTRGNETTO  PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATKOMBIRABATT   PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATKUNDENBONUS   PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATKZDIFFKUEND   PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATKZDIFFWG      PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATMITARBID      PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATMITARBRAB     PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATOEFFDIENSTNL  PIC S9(4) COMP.
             15 PI-DRKI0403-N-STPRDKTTXTFV      PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATPRODVARIANTE  PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATRISIKBRTGES   PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATRISIKBRTGWKZ  PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATRISIKNETGES   PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATRISIKNETGWKZ  PIC S9(4) COMP.
             15 PI-DRKI0403-N-STRSKNSCHRRT      PIC S9(4) COMP.
             15 PI-DRKI0403-N-STRSKNSCHRPLZ     PIC S9(4) COMP.
             15 PI-DRKI0403-N-STRSKNSCHRSTR     PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATSEBSTBETWKZ   PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATSELBSTBETEIL  PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATSOFORTRABATT  PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATSONSTIGE      PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATSUBTYPE       PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATTARIFSB       PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATTARIFSBWKZ    PIC S9(4) COMP.
             15 PI-DRKI0403-N-STVRSNDWG-KNR     PIC S9(4) COMP.
             15 PI-DRKI0403-N-STVRSNDWG-KTY     PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATVERSLSTID     PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATVSPNACHLASS   PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATZUKNETTOJBR   PIC S9(4) COMP.
             15 PI-DRKI0403-N-STZKNTTJBRWKZ     PIC S9(4) COMP.
             15 PI-DRKI0403-N-STZWGSCHLSSL      PIC S9(4) COMP.
             15 PI-DRKI0403-NI-SATZWEIGTEXT     PIC S9(4) COMP.
END   *

           05  PI-DRKI0403-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI0403-SATANNAHMEDAT   PIC  X(010).
             10 PI-DRKI0403-SATBESTMANAGKZ   PIC  X(0001).
             10 PI-DRKI0403-SATDAUERNACHLASS   PIC  X(0001).
             10 PI-DRKI0403-SATDIFFABLAUF   PIC  X(010).
             10 PI-DRKI0403-SATDIFFGES   PIC  X(0090).
             10 PI-DRKI0403-SATDIFFVNR   PIC  X(0040).
             10 PI-DRKI0403-SATEEGFKOSTENG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0403-SATEEGFKOSTENGWKZ   PIC  X(0001).
             10 PI-DRKI0403-SATEGKUENSTGS   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0403-SATEGKUENSTGSKZ   PIC  X(0001).
             10 PI-DRKI0403-SATEGPRIVATGH   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0403-SATEGPRIVATGHWKZ   PIC  X(0001).
             10 PI-DRKI0403-SATGLASBERECHID   PIC S9(4) COMP.
             10 PI-DRKI0403-SATGLASBERECHWERT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0403-SATGLASBERWERTWKZ   PIC  X(0001).
             10 PI-DRKI0403-SATHINWAUFSELBSTB   PIC  X(0001).
             10 PI-DRKI0403-SATJAHRBEITRWKZ   PIC  X(0001).
             10 PI-DRKI0403-SATJAHRESBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0403-SATJHRBNETTOWKZ   PIC  X(0001).
             10 PI-DRKI0403-SATJHRBTRGNETTO   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0403-SATKOMBIRABATT   PIC S9(4) COMP.
             10 PI-DRKI0403-SATKUNDENBONUS   PIC  X(0001).
             10 PI-DRKI0403-SATKZDIFFKUEND   PIC  X(0001).
             10 PI-DRKI0403-SATKZDIFFWG   PIC  X(0001).
             10 PI-DRKI0403-SATMITARBID   PIC  X(0012).
             10 PI-DRKI0403-SATMITARBRAB   PIC S9(4) COMP.
             10 PI-DRKI0403-SATOEFFDIENSTNL   PIC  X(0001).
             10 PI-DRKI0403-SATPRODUKTTEXTFV   PIC  X(0020).
             10 PI-DRKI0403-SATPRODVARIANTE   PIC  X(0030).
             10 PI-DRKI0403-SATRISIKBRTGES   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0403-SATRISIKBRTGWKZ   PIC  X(0001).
             10 PI-DRKI0403-SATRISIKNETGES   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0403-SATRISIKNETGWKZ   PIC  X(0001).
             10 PI-DRKI0403-SATRISKANSCHRORT   PIC  X(0030).
             10 PI-DRKI0403-SATRISKANSCHRPLZ   PIC  X(0006).
             10 PI-DRKI0403-SATRISKANSCHRSTR   PIC  X(0045).
             10 PI-DRKI0403-SATSEBSTBETWKZ   PIC  X(0001).
             10 PI-DRKI0403-SATSELBSTBETEIL   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0403-SATSOFORTRABATT   PIC  X(0002).
             10 PI-DRKI0403-SATSONSTIGE   PIC  X(0030).
             10 PI-DRKI0403-SATSUBTYPE   PIC  X(0001).
             10 PI-DRKI0403-SATTARIFSB   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0403-SATTARIFSBWKZ   PIC  X(0001).
             10 PI-DRKI0403-SATVERSANDWEG-KNR   PIC S9(4) COMP.
             10 PI-DRKI0403-SATVERSANDWEG-KTY   PIC S9(4) COMP.
             10 PI-DRKI0403-SATVERSLSTID   PIC S9(4) COMP.
             10 PI-DRKI0403-SATVSPNACHLASS   PIC  X(0001).
             10 PI-DRKI0403-SATZUKNETTOJBR   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0403-SATZUKNETTOJBRWKZ   PIC  X(0001).
             10 PI-DRKI0403-SATZWEIGSCHLUESSEL   PIC  X(0002).
             10 PI-DRKI0403-SATZWEIGTEXT   PIC  X(0003).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI0403-NI1.
           05 PI-DRKI0403-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 51.
       01  PI-DRKI0403-NI7.
           05 PI-DRKI0403-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 51.

      *
       01  PO-DRKI0403.
           05  PO-DRKI0403-RC                     PIC  X(003).
               88 PO-DRKI0403-RC-OK                  VALUE 'OK '.
               88 PO-DRKI0403-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

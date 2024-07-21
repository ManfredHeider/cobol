      *---------------------------------------------------------------*
      *    Standard fˆr alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0393.CPY  $
      *    $Revision:   2.13  $
      *
      *    $Log:   L:/mf/CPY/DRKI0393.CPv  $
      *
      *   Rev 2.13   Mar 30 2020 08:32:48   t13ae31
      *AKI01766: G81 Advocard neue Felder u.a.
      *
      *   Rev 2.12   Dec 09 2019 13:50:06   t13ae31
      *AKI01734: G81 Advocard Nettopreise Stornodatum UPKI0050 OZP
      *
      *   Rev 2.11   Jan 11 2016 14:55:56   t13ex78
      *AKI01404: AdvoCard - Kombirabatt und Kundenbonus trennen - Z
      *TKI0353
      *
      *   Rev 2.10   Aug 28 2013 13:18:34   t13ex19
      *AKI01249: AdvoCard - Sofortrabatt
      *
      *   Rev 2.9   Jul 21 2009 13:32:46   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 1.0   14 Jul 1998 17:04:30   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0393                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0393                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         26.04.2021 / 13:34:16 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI0393
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0393.
           05  PI-DRKI0393-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI0393-SATVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0393-SATVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI0393-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI0393-NI-SATANNAHMEDAT    PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATBESTMANAGKZ   PIC S9(4) COMP.
             15 PI-DRKI0393-N-STDRNCHLSS        PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATDIFFABLAUF    PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATDIFFGES       PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATDIFFVNR       PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATEEGFKOSTENG   PIC S9(4) COMP.
             15 PI-DRKI0393-N-STGFKSTNGWKZ      PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATEGKUENSTGS    PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATEGKUENSTGSKZ  PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATEGPRIVATGH    PIC S9(4) COMP.
             15 PI-DRKI0393-N-STGPRVTGHWKZ      PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATGLASBERECHID  PIC S9(4) COMP.
             15 PI-DRKI0393-N-STGLSBRCHWRT      PIC S9(4) COMP.
             15 PI-DRKI0393-N-STGLSBRWRTWKZ     PIC S9(4) COMP.
             15 PI-DRKI0393-N-STHNWFSLBSTB      PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATJAHRBEITRWKZ  PIC S9(4) COMP.
             15 PI-DRKI0393-N-STJHRSBTRG        PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATJHRBNETTOWKZ  PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATJHRBTRGNETTO  PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATKOMBIRABATT   PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATKUNDENBONUS   PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATKZDIFFKUEND   PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATKZDIFFWG      PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATMITARBID      PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATMITARBRAB     PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATOEFFDIENSTNL  PIC S9(4) COMP.
             15 PI-DRKI0393-N-STPRDKTTXTFV      PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATPRODVARIANTE  PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATRISIKBRTGES   PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATRISIKBRTGWKZ  PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATRISIKNETGES   PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATRISIKNETGWKZ  PIC S9(4) COMP.
             15 PI-DRKI0393-N-STRSKNSCHRRT      PIC S9(4) COMP.
             15 PI-DRKI0393-N-STRSKNSCHRPLZ     PIC S9(4) COMP.
             15 PI-DRKI0393-N-STRSKNSCHRSTR     PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATSEBSTBETWKZ   PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATSELBSTBETEIL  PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATSOFORTRABATT  PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATSONSTIGE      PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATSUBTYPE       PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATTARIFSB       PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATTARIFSBWKZ    PIC S9(4) COMP.
             15 PI-DRKI0393-N-STVRSNDWG-KNR     PIC S9(4) COMP.
             15 PI-DRKI0393-N-STVRSNDWG-KTY     PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATVERSLSTID     PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATVSPNACHLASS   PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATZUKNETTOJBR   PIC S9(4) COMP.
             15 PI-DRKI0393-N-STZKNTTJBRWKZ     PIC S9(4) COMP.
             15 PI-DRKI0393-N-STZWGSCHLSSL      PIC S9(4) COMP.
             15 PI-DRKI0393-NI-SATZWEIGTEXT     PIC S9(4) COMP.
END   *

           05  PI-DRKI0393-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI0393-SATANNAHMEDAT   PIC  X(010).
             10 PI-DRKI0393-SATBESTMANAGKZ   PIC  X(0001).
             10 PI-DRKI0393-SATDAUERNACHLASS   PIC  X(0001).
             10 PI-DRKI0393-SATDIFFABLAUF   PIC  X(010).
             10 PI-DRKI0393-SATDIFFGES   PIC  X(0090).
             10 PI-DRKI0393-SATDIFFVNR   PIC  X(0040).
             10 PI-DRKI0393-SATEEGFKOSTENG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0393-SATEEGFKOSTENGWKZ   PIC  X(0001).
             10 PI-DRKI0393-SATEGKUENSTGS   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0393-SATEGKUENSTGSKZ   PIC  X(0001).
             10 PI-DRKI0393-SATEGPRIVATGH   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0393-SATEGPRIVATGHWKZ   PIC  X(0001).
             10 PI-DRKI0393-SATGLASBERECHID   PIC S9(4) COMP.
             10 PI-DRKI0393-SATGLASBERECHWERT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0393-SATGLASBERWERTWKZ   PIC  X(0001).
             10 PI-DRKI0393-SATHINWAUFSELBSTB   PIC  X(0001).
             10 PI-DRKI0393-SATJAHRBEITRWKZ   PIC  X(0001).
             10 PI-DRKI0393-SATJAHRESBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0393-SATJHRBNETTOWKZ   PIC  X(0001).
             10 PI-DRKI0393-SATJHRBTRGNETTO   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0393-SATKOMBIRABATT   PIC S9(4) COMP.
             10 PI-DRKI0393-SATKUNDENBONUS   PIC  X(0001).
             10 PI-DRKI0393-SATKZDIFFKUEND   PIC  X(0001).
             10 PI-DRKI0393-SATKZDIFFWG   PIC  X(0001).
             10 PI-DRKI0393-SATMITARBID   PIC  X(0012).
             10 PI-DRKI0393-SATMITARBRAB   PIC S9(4) COMP.
             10 PI-DRKI0393-SATOEFFDIENSTNL   PIC  X(0001).
             10 PI-DRKI0393-SATPRODUKTTEXTFV   PIC  X(0020).
             10 PI-DRKI0393-SATPRODVARIANTE   PIC  X(0030).
             10 PI-DRKI0393-SATRISIKBRTGES   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0393-SATRISIKBRTGWKZ   PIC  X(0001).
             10 PI-DRKI0393-SATRISIKNETGES   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0393-SATRISIKNETGWKZ   PIC  X(0001).
             10 PI-DRKI0393-SATRISKANSCHRORT   PIC  X(0030).
             10 PI-DRKI0393-SATRISKANSCHRPLZ   PIC  X(0006).
             10 PI-DRKI0393-SATRISKANSCHRSTR   PIC  X(0045).
             10 PI-DRKI0393-SATSEBSTBETWKZ   PIC  X(0001).
             10 PI-DRKI0393-SATSELBSTBETEIL   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0393-SATSOFORTRABATT   PIC  X(0002).
             10 PI-DRKI0393-SATSONSTIGE   PIC  X(0030).
             10 PI-DRKI0393-SATSUBTYPE   PIC  X(0001).
             10 PI-DRKI0393-SATTARIFSB   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0393-SATTARIFSBWKZ   PIC  X(0001).
             10 PI-DRKI0393-SATVERSANDWEG-KNR   PIC S9(4) COMP.
             10 PI-DRKI0393-SATVERSANDWEG-KTY   PIC S9(4) COMP.
             10 PI-DRKI0393-SATVERSLSTID   PIC S9(4) COMP.
             10 PI-DRKI0393-SATVSPNACHLASS   PIC  X(0001).
             10 PI-DRKI0393-SATZUKNETTOJBR   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0393-SATZUKNETTOJBRWKZ   PIC  X(0001).
             10 PI-DRKI0393-SATZWEIGSCHLUESSEL   PIC  X(0002).
             10 PI-DRKI0393-SATZWEIGTEXT   PIC  X(0003).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI0393-NI1.
           05 PI-DRKI0393-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 51.
       01  PI-DRKI0393-NI7.
           05 PI-DRKI0393-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 51.
      *
       01  PO-DRKI0393.
           05  PO-DRKI0393-RC                     PIC  X(003).
               88 PO-DRKI0393-RC-OK               VALUE 'OK '.
               88 PO-DRKI0393-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

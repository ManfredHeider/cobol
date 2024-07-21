      *---------------------------------------------------------------*
      *    Standard fˆr alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0392.CPY  $
      *    $Revision:   2.15  $
      *
      *    $Log:   L:/mf/CPY/DRKI0392.CPv  $
      *
      *   Rev 2.15   Mar 30 2020 08:34:12   t13ae31
      *AKI01766: G81 Advocard neue Felder u.a.
      *
      *   Rev 2.14   Dec 09 2019 13:49:22   t13ae31
      *AKI01734: G81 Advocard Nettopreise Stornodatum UPKI0050 OZP
      *
      *   Rev 2.13   Jan 11 2016 14:55:48   t13ex78
      *AKI01404: AdvoCard - Kombirabatt und Kundenbonus trennen - Z
      *TKI0353
      *
      *   Rev 2.12   Aug 28 2013 13:18:38   t13ex19
      *AKI01249: AdvoCard - Sofortrabatt
      *
      *   Rev 2.11   Jul 21 2009 13:32:38   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 1.0   14 Jul 1998 17:04:26   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0392                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0392                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         26.04.2021 / 13:34:13 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI0392
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0392.
           05  PI-DRKI0392-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI0392-SATVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0392-SATVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI0392.
           05  PO-DRKI0392-RC                     PIC  X(003).
               88 PO-DRKI0392-RC-OK               VALUE 'OK '.
               88 PO-DRKI0392-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI0392-NI.
               10 NI-DRKI0392-TAB.
                  15 DRKI0392-ELE
                     PIC S9(04) COMP OCCURS 51 TIMES.
               10 NI-DRKI0392 REDEFINES NI-DRKI0392-TAB.

ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI0392-NI-SATANNAHMEDAT   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATBESTMANAGKZ   PIC S9(4) COMP.
             15 PO-DRKI0392-N-STDRNCHLSS   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATDIFFABLAUF   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATDIFFGES   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATDIFFVNR   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATEEGFKOSTENG   PIC S9(4) COMP.
             15 PO-DRKI0392-N-STGFKSTNGWKZ   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATEGKUENSTGS   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATEGKUENSTGSKZ   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATEGPRIVATGH   PIC S9(4) COMP.
             15 PO-DRKI0392-N-STGPRVTGHWKZ   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATGLASBERECHID   PIC S9(4) COMP.
             15 PO-DRKI0392-N-STGLSBRCHWRT   PIC S9(4) COMP.
             15 PO-DRKI0392-N-STGLSBRWRTWKZ   PIC S9(4) COMP.
             15 PO-DRKI0392-N-STHNWFSLBSTB   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATJAHRBEITRWKZ   PIC S9(4) COMP.
             15 PO-DRKI0392-N-STJHRSBTRG   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATJHRBNETTOWKZ   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATJHRBTRGNETTO   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATKOMBIRABATT   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATKUNDENBONUS   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATKZDIFFKUEND   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATKZDIFFWG   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATMITARBID   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATMITARBRAB   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATOEFFDIENSTNL   PIC S9(4) COMP.
             15 PO-DRKI0392-N-STPRDKTTXTFV   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATPRODVARIANTE   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATRISIKBRTGES   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATRISIKBRTGWKZ   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATRISIKNETGES   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATRISIKNETGWKZ   PIC S9(4) COMP.
             15 PO-DRKI0392-N-STRSKNSCHRRT   PIC S9(4) COMP.
             15 PO-DRKI0392-N-STRSKNSCHRPLZ   PIC S9(4) COMP.
             15 PO-DRKI0392-N-STRSKNSCHRSTR   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATSEBSTBETWKZ   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATSELBSTBETEIL   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATSOFORTRABATT   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATSONSTIGE   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATSUBTYPE   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATTARIFSB   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATTARIFSBWKZ   PIC S9(4) COMP.
             15 PO-DRKI0392-N-STVRSNDWG-KNR   PIC S9(4) COMP.
             15 PO-DRKI0392-N-STVRSNDWG-KTY   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATVERSLSTID   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATVSPNACHLASS   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATZUKNETTOJBR   PIC S9(4) COMP.
             15 PO-DRKI0392-N-STZKNTTJBRWKZ   PIC S9(4) COMP.
             15 PO-DRKI0392-N-STZWGSCHLSSL   PIC S9(4) COMP.
             15 PO-DRKI0392-NI-SATZWEIGTEXT   PIC S9(4) COMP.
END   *

           05  PO-DRKI0392-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI0392-SATANNAHMEDAT   PIC  X(010).
             10 PO-DRKI0392-SATBESTMANAGKZ   PIC  X(0001).
             10 PO-DRKI0392-SATDAUERNACHLASS   PIC  X(0001).
             10 PO-DRKI0392-SATDIFFABLAUF   PIC  X(010).
             10 PO-DRKI0392-SATDIFFGES   PIC  X(0090).
             10 PO-DRKI0392-SATDIFFVNR   PIC  X(0040).
             10 PO-DRKI0392-SATEEGFKOSTENG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0392-SATEEGFKOSTENGWKZ   PIC  X(0001).
             10 PO-DRKI0392-SATEGKUENSTGS   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0392-SATEGKUENSTGSKZ   PIC  X(0001).
             10 PO-DRKI0392-SATEGPRIVATGH   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0392-SATEGPRIVATGHWKZ   PIC  X(0001).
             10 PO-DRKI0392-SATGLASBERECHID   PIC S9(4) COMP.
             10 PO-DRKI0392-SATGLASBERECHWERT   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0392-SATGLASBERWERTWKZ   PIC  X(0001).
             10 PO-DRKI0392-SATHINWAUFSELBSTB   PIC  X(0001).
             10 PO-DRKI0392-SATJAHRBEITRWKZ   PIC  X(0001).
             10 PO-DRKI0392-SATJAHRESBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0392-SATJHRBNETTOWKZ   PIC  X(0001).
             10 PO-DRKI0392-SATJHRBTRGNETTO   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0392-SATKOMBIRABATT   PIC S9(4) COMP.
             10 PO-DRKI0392-SATKUNDENBONUS   PIC  X(0001).
             10 PO-DRKI0392-SATKZDIFFKUEND   PIC  X(0001).
             10 PO-DRKI0392-SATKZDIFFWG   PIC  X(0001).
             10 PO-DRKI0392-SATMITARBID   PIC  X(0012).
             10 PO-DRKI0392-SATMITARBRAB   PIC S9(4) COMP.
             10 PO-DRKI0392-SATOEFFDIENSTNL   PIC  X(0001).
             10 PO-DRKI0392-SATPRODUKTTEXTFV   PIC  X(0020).
             10 PO-DRKI0392-SATPRODVARIANTE   PIC  X(0030).
             10 PO-DRKI0392-SATRISIKBRTGES   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0392-SATRISIKBRTGWKZ   PIC  X(0001).
             10 PO-DRKI0392-SATRISIKNETGES   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0392-SATRISIKNETGWKZ   PIC  X(0001).
             10 PO-DRKI0392-SATRISKANSCHRORT   PIC  X(0030).
             10 PO-DRKI0392-SATRISKANSCHRPLZ   PIC  X(0006).
             10 PO-DRKI0392-SATRISKANSCHRSTR   PIC  X(0045).
             10 PO-DRKI0392-SATSEBSTBETWKZ   PIC  X(0001).
             10 PO-DRKI0392-SATSELBSTBETEIL   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0392-SATSOFORTRABATT   PIC  X(0002).
             10 PO-DRKI0392-SATSONSTIGE   PIC  X(0030).
             10 PO-DRKI0392-SATSUBTYPE   PIC  X(0001).
             10 PO-DRKI0392-SATTARIFSB   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0392-SATTARIFSBWKZ   PIC  X(0001).
             10 PO-DRKI0392-SATVERSANDWEG-KNR   PIC S9(4) COMP.
             10 PO-DRKI0392-SATVERSANDWEG-KTY   PIC S9(4) COMP.
             10 PO-DRKI0392-SATVERSLSTID   PIC S9(4) COMP.
             10 PO-DRKI0392-SATVSPNACHLASS   PIC  X(0001).
             10 PO-DRKI0392-SATZUKNETTOJBR   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0392-SATZUKNETTOJBRWKZ   PIC  X(0001).
             10 PO-DRKI0392-SATZWEIGSCHLUESSEL   PIC  X(0002).
             10 PO-DRKI0392-SATZWEIGTEXT   PIC  X(0003).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.07.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2644.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2644.CPv  $
      *
      *   Rev 2.7   Nov 16 2017 11:22:16   t13ex78
      *AKI01532: Programme und Copies - OZPs
      *
      *   Rev 2.6   Sep 20 2017 08:11:04   t13ex78
      *AKI01532: Programme und Copies - OZPs
      *
      *   Rev 2.5   Sep 04 2017 09:10:34   t13ex78
      *AKI01532: Programme und Copies
      *
      *   Rev 2.4   Aug 23 2012 10:33:54   t13ex39
      *AKI01141:  O  Z  P  DTA-Erweiterung Fondsdepot Bank "WpHG"
      *
      *   Rev 2.3   Jul 25 2012 13:01:36   t13ex39
      *AKI01141:  O  Z  P  DTA-Erweiterung Fondsdepot Bank "WpHG"
      *
      *   Rev 2.2   Jul 23 2012 08:08:10   t13ex39
      *AKI01141:  O  Z  P  DTA-Erweiterung Fondsdepot Bank "WpHG"
      *
      *   Rev 2.1   Jul 20 2012 11:00:28   t13ex39
      *AKI01141:  O  Z  P  DTA-Erweiterung Fondsdepot Bank "WpHG"
      *
      *   Rev 2.0   Jul 20 2012 07:49:26 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2644                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2644                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         20.09.2017 / 08:04:59 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2644
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2644.
           05  PI-DRKI2644-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2644-WPHGVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI2644.
           05  PO-DRKI2644-RC                     PIC  X(003).
               88 PO-DRKI2644-RC-OK               VALUE 'OK '.
               88 PO-DRKI2644-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI2644-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2644-WPDIENSTANLBER   PIC  X(0001).
             10 PO-DRKI2644-WPHGALSKEINKZIEL   PIC  X(0001).
             10 PO-DRKI2644-WPHGALSPRVERMAUF   PIC  X(0001).
             10 PO-DRKI2644-WPHGALSRENTVORS   PIC  X(0001).
             10 PO-DRKI2644-WPHGALSWOHNEIGEN   PIC  X(0001).
             10 PO-DRKI2644-WPHGANLDAUER-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGANLZIEL-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGANZKAUF-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGASGESRIVORS   PIC  X(0001).
             10 PO-DRKI2644-WPHGAUSGABEN-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGBERUF-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGDATUNTERSCHR   PIC  X(010).
             10 PO-DRKI2644-WPHGFREIELIQ-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGGESCHKRBASIS   PIC  X(0001).
             10 PO-DRKI2644-WPHGGPARTNER-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGHOEHEKRRAHM   PIC S9(6)V9(2) COMP-3.
             10 PO-DRKI2644-WPHGIMMOVERM-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGKAPVERM-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGKEINEWPDNSTL   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER1RENTENF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER2DMISCHF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER2IMMOF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER2KAPGESPR   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER2RENTENF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER2STRUKINV   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER2STRUKTWP   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER3AKTIENF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER3ALTINV   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER3KAPGESPR   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER3KAPGESP90   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER3RENTENF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER3STRUKTWP   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER4AKTFETF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER4AKTIENF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER4ALTINV   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER4GELDMF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER4IMMOF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER4MISCHF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER4PROD14   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER4PROD75   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER4RENTEETF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER4RENTENF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER4ROHSTF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER4STRUKPR   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER4STRUKTWP   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER4SYNETF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER5AITERMIN   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER5AKTIENF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER5RENTENF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER5ROHF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER5STRUKRF   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER5STRUKTWP   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER5SYNETFA   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER5SYNETFR   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER6HEBELPR   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER6LONGPOS   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER7DERIVATE   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER7DEVTERM   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER7FUTURES   PIC  X(0001).
             10 PO-DRKI2644-WPHGKER7SHORTPOS   PIC  X(0001).
             10 PO-DRKI2644-WPHGKE1DAUER-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGKE2DAUER-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGKE3DAUER-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGKE4DAUER-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGKE5DAUER-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGKE6DAUER-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGKE7DAUER-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGKTOGUTH-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGNETTVERM-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGNICHSELB-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGORDERGR-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGSELBST-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGSONSVERM-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGVERBINDL-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGWEITERE-KNR   PIC S9(4) COMP.
             10 PO-DRKI2644-WPHGWPDBERFREI   PIC  X(0001).
             10 PO-DRKI2644-WPHGWPDVERMVERW   PIC  X(0001).
             10 PO-DRKI2644-WPHGWPVERM-KNR   PIC S9(4) COMP.
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************
 


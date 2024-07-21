      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.07.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2646.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2646.CPv  $
      *
      *   Rev 2.7   Nov 16 2017 11:22:32   t13ex78
      *AKI01532: Programme und Copies - OZPs
      *
      *   Rev 2.6   Sep 20 2017 08:11:18   t13ex78
      *AKI01532: Programme und Copies - OZPs
      *
      *   Rev 2.5   Sep 04 2017 09:12:02   t13ex78
      *AKI01532: Programme und Copies
      *
      *   Rev 2.4   Aug 23 2012 10:34:32   t13ex39
      *AKI01141:  O  Z  P  DTA-Erweiterung Fondsdepot Bank "WpHG"
      *
      *   Rev 2.3   Jul 25 2012 13:01:52   t13ex39
      *AKI01141:  O  Z  P  DTA-Erweiterung Fondsdepot Bank "WpHG"
      *
      *   Rev 2.2   Jul 23 2012 08:08:28   t13ex39
      *AKI01141:  O  Z  P  DTA-Erweiterung Fondsdepot Bank "WpHG"
      *
      *   Rev 2.1   Jul 20 2012 11:00:48   t13ex39
      *AKI01141:  O  Z  P  DTA-Erweiterung Fondsdepot Bank "WpHG"
      *
      *   Rev 2.0   Jul 20 2012 07:49:34 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2646                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2646                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         20.09.2017 / 08:05:05 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2646
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2646.
           05  PI-DRKI2646-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2646-WPHGVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI2646-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2646-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2646-WPDIENSTANLBER   PIC  X(0001).
             10 PI-DRKI2646-WPHGALSKEINKZIEL   PIC  X(0001).
             10 PI-DRKI2646-WPHGALSPRVERMAUF   PIC  X(0001).
             10 PI-DRKI2646-WPHGALSRENTVORS   PIC  X(0001).
             10 PI-DRKI2646-WPHGALSWOHNEIGEN   PIC  X(0001).
             10 PI-DRKI2646-WPHGANLDAUER-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGANLZIEL-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGANZKAUF-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGASGESRIVORS   PIC  X(0001).
             10 PI-DRKI2646-WPHGAUSGABEN-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGBERUF-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGDATUNTERSCHR   PIC  X(010).
             10 PI-DRKI2646-WPHGFREIELIQ-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGGESCHKRBASIS   PIC  X(0001).
             10 PI-DRKI2646-WPHGGPARTNER-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGHOEHEKRRAHM   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI2646-WPHGIMMOVERM-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGKAPVERM-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGKEINEWPDNSTL   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER1RENTENF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER2DMISCHF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER2IMMOF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER2KAPGESPR   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER2RENTENF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER2STRUKINV   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER2STRUKTWP   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER3AKTIENF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER3ALTINV   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER3KAPGESPR   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER3KAPGESP90   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER3RENTENF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER3STRUKTWP   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER4AKTFETF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER4AKTIENF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER4ALTINV   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER4GELDMF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER4IMMOF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER4MISCHF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER4PROD14   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER4PROD75   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER4RENTEETF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER4RENTENF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER4ROHSTF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER4STRUKPR   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER4STRUKTWP   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER4SYNETF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER5AITERMIN   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER5AKTIENF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER5RENTENF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER5ROHF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER5STRUKRF   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER5STRUKTWP   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER5SYNETFA   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER5SYNETFR   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER6HEBELPR   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER6LONGPOS   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER7DERIVATE   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER7DEVTERM   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER7FUTURES   PIC  X(0001).
             10 PI-DRKI2646-WPHGKER7SHORTPOS   PIC  X(0001).
             10 PI-DRKI2646-WPHGKE1DAUER-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGKE2DAUER-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGKE3DAUER-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGKE4DAUER-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGKE5DAUER-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGKE6DAUER-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGKE7DAUER-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGKTOGUTH-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGNETTVERM-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGNICHSELB-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGORDERGR-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGSELBST-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGSONSVERM-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGVERBINDL-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGWEITERE-KNR   PIC S9(4) COMP.
             10 PI-DRKI2646-WPHGWPDBERFREI   PIC  X(0001).
             10 PI-DRKI2646-WPHGWPDVERMVERW   PIC  X(0001).
             10 PI-DRKI2646-WPHGWPVERM-KNR   PIC S9(4) COMP.
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2646-NI1.
           05 PI-DRKI2646-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI2646-NI7.
           05 PI-DRKI2646-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI2646.
           05  PO-DRKI2646-RC                     PIC  X(003).
               88 PO-DRKI2646-RC-OK               VALUE 'OK '.
               88 PO-DRKI2646-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
 


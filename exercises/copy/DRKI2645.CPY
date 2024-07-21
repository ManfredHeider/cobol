      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.07.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2645.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2645.CPv  $
      *
      *   Rev 2.7   Nov 16 2017 11:22:24   t13ex78
      *AKI01532: Programme und Copies - OZPs
      *
      *   Rev 2.6   Sep 20 2017 08:11:10   t13ex78
      *AKI01532: Programme und Copies - OZPs
      *
      *   Rev 2.5   Sep 04 2017 09:11:54   t13ex78
      *AKI01532: Programme und Copies
      *
      *   Rev 2.4   Aug 23 2012 10:34:14   t13ex39
      *AKI01141:  O  Z  P  DTA-Erweiterung Fondsdepot Bank "WpHG"
      *
      *   Rev 2.3   Jul 25 2012 13:01:44   t13ex39
      *AKI01141:  O  Z  P  DTA-Erweiterung Fondsdepot Bank "WpHG"
      *
      *   Rev 2.2   Jul 23 2012 08:08:20   t13ex39
      *AKI01141:  O  Z  P  DTA-Erweiterung Fondsdepot Bank "WpHG"
      *
      *   Rev 2.1   Jul 20 2012 11:00:38   t13ex39
      *AKI01141:  O  Z  P  DTA-Erweiterung Fondsdepot Bank "WpHG"
      *
      *   Rev 2.0   Jul 20 2012 07:49:30 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2645                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2645                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         20.09.2017 / 08:05:02 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2645
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2645.
           05  PI-DRKI2645-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2645-WPHGVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI2645-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2645-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2645-WPDIENSTANLBER   PIC  X(0001).
             10 PI-DRKI2645-WPHGALSKEINKZIEL   PIC  X(0001).
             10 PI-DRKI2645-WPHGALSPRVERMAUF   PIC  X(0001).
             10 PI-DRKI2645-WPHGALSRENTVORS   PIC  X(0001).
             10 PI-DRKI2645-WPHGALSWOHNEIGEN   PIC  X(0001).
             10 PI-DRKI2645-WPHGANLDAUER-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGANLZIEL-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGANZKAUF-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGASGESRIVORS   PIC  X(0001).
             10 PI-DRKI2645-WPHGAUSGABEN-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGBERUF-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGDATUNTERSCHR   PIC  X(010).
             10 PI-DRKI2645-WPHGFREIELIQ-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGGESCHKRBASIS   PIC  X(0001).
             10 PI-DRKI2645-WPHGGPARTNER-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGHOEHEKRRAHM   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI2645-WPHGIMMOVERM-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGKAPVERM-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGKEINEWPDNSTL   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER1RENTENF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER2DMISCHF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER2IMMOF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER2KAPGESPR   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER2RENTENF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER2STRUKINV   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER2STRUKTWP   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER3AKTIENF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER3ALTINV   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER3KAPGESPR   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER3KAPGESP90   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER3RENTENF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER3STRUKTWP   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER4AKTFETF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER4AKTIENF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER4ALTINV   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER4GELDMF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER4IMMOF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER4MISCHF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER4PROD14   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER4PROD75   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER4RENTEETF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER4RENTENF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER4ROHSTF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER4STRUKPR   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER4STRUKTWP   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER4SYNETF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER5AITERMIN   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER5AKTIENF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER5RENTENF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER5ROHF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER5STRUKRF   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER5STRUKTWP   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER5SYNETFA   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER5SYNETFR   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER6HEBELPR   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER6LONGPOS   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER7DERIVATE   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER7DEVTERM   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER7FUTURES   PIC  X(0001).
             10 PI-DRKI2645-WPHGKER7SHORTPOS   PIC  X(0001).
             10 PI-DRKI2645-WPHGKE1DAUER-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGKE2DAUER-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGKE3DAUER-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGKE4DAUER-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGKE5DAUER-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGKE6DAUER-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGKE7DAUER-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGKTOGUTH-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGNETTVERM-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGNICHSELB-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGORDERGR-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGSELBST-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGSONSVERM-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGVERBINDL-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGWEITERE-KNR   PIC S9(4) COMP.
             10 PI-DRKI2645-WPHGWPDBERFREI   PIC  X(0001).
             10 PI-DRKI2645-WPHGWPDVERMVERW   PIC  X(0001).
             10 PI-DRKI2645-WPHGWPVERM-KNR   PIC S9(4) COMP.
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2645-NI1.
           05 PI-DRKI2645-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI2645-NI7.
           05 PI-DRKI2645-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.
 
      *
       01  PO-DRKI2645.
           05  PO-DRKI2645-RC                     PIC  X(003).
               88 PO-DRKI2645-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2645-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
 


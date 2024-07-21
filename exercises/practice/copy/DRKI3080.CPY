      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.09.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3080.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3080.CPv  $
      *
      *   Rev 2.4   May 24 2018 10:24:20   t13ex78
      *AKI01587: OZP's
      *
      *   Rev 2.3   Dec 06 2016 15:12:40   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.2   Nov 28 2016 12:15:28   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.1   Nov 28 2016 11:34:04   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.0   Sep 1 2016 12:05:23 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3080                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3080                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         16.05.2018 / 10:04:35 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3080
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3080.
           05  PI-DRKI3080-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3080-VBSPERSONENID   PIC  X(0013).
END   *
 
           05  PI-DRKI3080-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI3080-NI-VBSAUSLGESCH     PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSAUSLPRIV      PIC S9(4) COMP.
             15 PI-DRKI3080-N-VBSSTRTTSDTM      PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSEDIDATUM      PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSEDISTATUS     PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSFAXGESCH      PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSFAXPRIVAT     PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSFONGESCH      PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSFONPRIVAT     PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSGEBGRUENDDAT  PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSMANDANTNRALT  PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSMOBILGESCH    PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSMOBILPRIVAT   PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSORTGESCH      PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSORTPRIV       PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSPLZGESCH      PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSPLZPRIV       PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSPROVSATZ      PIC S9(4) COMP.
             15 PI-DRKI3080-N-VBSRCKSTFDTM      PIC S9(4) COMP.
             15 PI-DRKI3080-N-VBSSTRSSNRGSCH    PIC S9(4) COMP.
             15 PI-DRKI3080-N-VBSSTRSSNRPRV     PIC S9(4) COMP.
             15 PI-DRKI3080-N-VBSSTRKTRSTF      PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSUMSTUFDATUM   PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSVORGESPERSID  PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSVORNAME       PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSZUSTELLGESCH  PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBSZUSTELLPRIV   PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBS34CDAT        PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBS34DREGISTNR   PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBS34FDAT        PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBS34FREGISTNR   PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBS34IDAT        PIC S9(4) COMP.
             15 PI-DRKI3080-NI-VBS34IREGISTNR   PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3080-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3080-VBSANREDE   PIC S9(4) COMP.
             10 PI-DRKI3080-VBSAUSLGESCH   PIC S9(4) COMP.
             10 PI-DRKI3080-VBSAUSLPRIV   PIC S9(4) COMP.
             10 PI-DRKI3080-VBSAUSTRITTSDATUM   PIC  X(010).
             10 PI-DRKI3080-VBSDIREKTION   PIC S9(4) COMP.
             10 PI-DRKI3080-VBSEDIDATUM   PIC  X(010).
             10 PI-DRKI3080-VBSEDISTATUS   PIC  X(0001).
             10 PI-DRKI3080-VBSFAXGESCH   PIC  X(0026).
             10 PI-DRKI3080-VBSFAXPRIVAT   PIC  X(0026).
             10 PI-DRKI3080-VBSFONGESCH   PIC  X(0026).
             10 PI-DRKI3080-VBSFONPRIVAT   PIC  X(0026).
             10 PI-DRKI3080-VBSGEBGRUENDDAT   PIC  X(010).
             10 PI-DRKI3080-VBSMANDANTNR   PIC S9(9) COMP.
             10 PI-DRKI3080-VBSMANDANTNRALT   PIC S9(9) COMP.
             10 PI-DRKI3080-VBSMITARBEITERNR   PIC S9(9) COMP.
             10 PI-DRKI3080-VBSMOBILGESCH   PIC  X(0026).
             10 PI-DRKI3080-VBSMOBILPRIVAT   PIC  X(0026).
             10 PI-DRKI3080-VBSNAME   PIC  X(0040).
             10 PI-DRKI3080-VBSORTGESCH   PIC  X(0040).
             10 PI-DRKI3080-VBSORTPRIV   PIC  X(0040).
             10 PI-DRKI3080-VBSPLZGESCH   PIC  X(0009).
             10 PI-DRKI3080-VBSPLZPRIV   PIC  X(0009).
             10 PI-DRKI3080-VBSPROVSATZ   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI3080-VBSRUECKSTUFDATUM   PIC  X(010).
             10 PI-DRKI3080-VBSSTRASSENRGESCH   PIC  X(0040).
             10 PI-DRKI3080-VBSSTRASSENRPRIV   PIC  X(0040).
             10 PI-DRKI3080-VBSSTRUKTURSTUFE   PIC S9(9) COMP.
             10 PI-DRKI3080-VBSUMSTUFDATUM   PIC  X(010).
             10 PI-DRKI3080-VBSVBARTKZ   PIC  X(0001).
             10 PI-DRKI3080-VBSVORGESPERSID   PIC  X(0013).
             10 PI-DRKI3080-VBSVORNAME   PIC  X(0020).
             10 PI-DRKI3080-VBSZUSTELLGESCH   PIC S9(4) COMP.
             10 PI-DRKI3080-VBSZUSTELLPRIV   PIC S9(4) COMP.
             10 PI-DRKI3080-VBS34CDAT   PIC  X(010).
             10 PI-DRKI3080-VBS34DREGISTNR   PIC  X(0015).
             10 PI-DRKI3080-VBS34FDAT   PIC  X(010).
             10 PI-DRKI3080-VBS34FREGISTNR   PIC  X(0015).
             10 PI-DRKI3080-VBS34IDAT   PIC  X(010).
             10 PI-DRKI3080-VBS34IREGISTNR   PIC  X(0015).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3080-NI1.
           05 PI-DRKI3080-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 33.
       01  PI-DRKI3080-NI7.
           05 PI-DRKI3080-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 33.
 
      *
       01  PO-DRKI3080.
           05  PO-DRKI3080-RC                     PIC  X(003).
               88 PO-DRKI3080-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3080-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************


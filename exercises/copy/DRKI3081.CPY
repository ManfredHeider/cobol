      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.09.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3081.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3081.CPv  $
      *
      *   Rev 2.4   May 24 2018 10:24:26   t13ex78
      *AKI01587: OZP's
      *
      *   Rev 2.3   Dec 06 2016 15:12:48   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.2   Nov 28 2016 12:15:38   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.1   Nov 28 2016 11:34:14   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.0   Sep 1 2016 12:05:28 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3081                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3081                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         16.05.2018 / 10:04:38 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3081
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3081.
           05  PI-DRKI3081-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3081-VBSMITARBEITERNR   PIC S9(9) COMP.
END   *
 
           05  PI-DRKI3081-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI3081-NI-VBSANREDE        PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSAUSLGESCH     PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSAUSLPRIV      PIC S9(4) COMP.
             15 PI-DRKI3081-N-VBSSTRTTSDTM      PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSDIREKTION     PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSEDIDATUM      PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSEDISTATUS     PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSFAXGESCH      PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSFAXPRIVAT     PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSFONGESCH      PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSFONPRIVAT     PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSGEBGRUENDDAT  PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSMANDANTNR     PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSMANDANTNRALT  PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSMOBILGESCH    PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSMOBILPRIVAT   PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSNAME          PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSORTGESCH      PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSORTPRIV       PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSPERSONENID    PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSPLZGESCH      PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSPLZPRIV       PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSPROVSATZ      PIC S9(4) COMP.
             15 PI-DRKI3081-N-VBSRCKSTFDTM      PIC S9(4) COMP.
             15 PI-DRKI3081-N-VBSSTRSSNRGSCH    PIC S9(4) COMP.
             15 PI-DRKI3081-N-VBSSTRSSNRPRV     PIC S9(4) COMP.
             15 PI-DRKI3081-N-VBSSTRKTRSTF      PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSUMSTUFDATUM   PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSVORGESPERSID  PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSVORNAME       PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSZUSTELLGESCH  PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBSZUSTELLPRIV   PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBS34CDAT        PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBS34DREGISTNR   PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBS34FDAT        PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBS34FREGISTNR   PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBS34IDAT        PIC S9(4) COMP.
             15 PI-DRKI3081-NI-VBS34IREGISTNR   PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3081-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3081-VBSANREDE   PIC S9(4) COMP.
             10 PI-DRKI3081-VBSAUSLGESCH   PIC S9(4) COMP.
             10 PI-DRKI3081-VBSAUSLPRIV   PIC S9(4) COMP.
             10 PI-DRKI3081-VBSAUSTRITTSDATUM   PIC  X(010).
             10 PI-DRKI3081-VBSDIREKTION   PIC S9(4) COMP.
             10 PI-DRKI3081-VBSEDIDATUM   PIC  X(010).
             10 PI-DRKI3081-VBSEDISTATUS   PIC  X(0001).
             10 PI-DRKI3081-VBSFAXGESCH   PIC  X(0026).
             10 PI-DRKI3081-VBSFAXPRIVAT   PIC  X(0026).
             10 PI-DRKI3081-VBSFONGESCH   PIC  X(0026).
             10 PI-DRKI3081-VBSFONPRIVAT   PIC  X(0026).
             10 PI-DRKI3081-VBSGEBGRUENDDAT   PIC  X(010).
             10 PI-DRKI3081-VBSMANDANTNR   PIC S9(9) COMP.
             10 PI-DRKI3081-VBSMANDANTNRALT   PIC S9(9) COMP.
             10 PI-DRKI3081-VBSMOBILGESCH   PIC  X(0026).
             10 PI-DRKI3081-VBSMOBILPRIVAT   PIC  X(0026).
             10 PI-DRKI3081-VBSNAME   PIC  X(0040).
             10 PI-DRKI3081-VBSORTGESCH   PIC  X(0040).
             10 PI-DRKI3081-VBSORTPRIV   PIC  X(0040).
             10 PI-DRKI3081-VBSPERSONENID   PIC  X(0013).
             10 PI-DRKI3081-VBSPLZGESCH   PIC  X(0009).
             10 PI-DRKI3081-VBSPLZPRIV   PIC  X(0009).
             10 PI-DRKI3081-VBSPROVSATZ   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI3081-VBSRUECKSTUFDATUM   PIC  X(010).
             10 PI-DRKI3081-VBSSTRASSENRGESCH   PIC  X(0040).
             10 PI-DRKI3081-VBSSTRASSENRPRIV   PIC  X(0040).
             10 PI-DRKI3081-VBSSTRUKTURSTUFE   PIC S9(9) COMP.
             10 PI-DRKI3081-VBSUMSTUFDATUM   PIC  X(010).
             10 PI-DRKI3081-VBSVBARTKZ   PIC  X(0001).
             10 PI-DRKI3081-VBSVORGESPERSID   PIC  X(0013).
             10 PI-DRKI3081-VBSVORNAME   PIC  X(0020).
             10 PI-DRKI3081-VBSZUSTELLGESCH   PIC S9(4) COMP.
             10 PI-DRKI3081-VBSZUSTELLPRIV   PIC S9(4) COMP.
             10 PI-DRKI3081-VBS34CDAT   PIC  X(010).
             10 PI-DRKI3081-VBS34DREGISTNR   PIC  X(0015).
             10 PI-DRKI3081-VBS34FDAT   PIC  X(010).
             10 PI-DRKI3081-VBS34FREGISTNR   PIC  X(0015).
             10 PI-DRKI3081-VBS34IDAT   PIC  X(010).
             10 PI-DRKI3081-VBS34IREGISTNR   PIC  X(0015).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3081-NI1.
           05 PI-DRKI3081-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 38.
       01  PI-DRKI3081-NI7.
           05 PI-DRKI3081-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 38.
      *
       01  PO-DRKI3081.
           05  PO-DRKI3081-RC                     PIC  X(003).
               88 PO-DRKI3081-RC-OK               VALUE 'OK '.
               88 PO-DRKI3081-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************


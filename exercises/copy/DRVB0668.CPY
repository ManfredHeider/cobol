      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  Januar 2003                               
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRVB0668.CPY  $                                         

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRVB0668.CPv  $                                
      *
      *   Rev 2.1   Apr 05 2004 11:41:02   t13ex22
      *AAS00454: Fkt. für Startseite VB-Portal
      *                                                                 
      *   Rev 2.0   Feb 17 2003 10:58:52   p13sp16                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      *---------------------------------------------------------------* 
      *    COPY-MEMBER: DRVB0668                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0059                        * 
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        * 
      *    Zu einer gegebenen VB-Nr und einem gegebenen Stichtag      * 
      *    wird der letzte JEQF (12-Monate-EQF Eigen, Gruppe, Gesamt) * 
      *    ermittelt                                                  * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: U. Boll                                         *  
      *    DATUM:    19.08.2003                                       * 
      ***************************************************************** 
      *    VERSION  1                                                   
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * --------+--------+----+---------------------------+----------*  
      * AAS00425!19.08.03! 2.1! Neu, zu gegebenen VB und  ! U. Boll  *  
      *         !        !    ! Stichtag den jew JEQF erm.!          *  
      * --------+--------+----+---------------------------+----------*  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *  
      *    FÜR DIE FUNKTION DRVB0668                                 *  
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *  
      *            DEM ERR-ERROR-BEREICH UND                         *  
      *            DEM STANDARD-STATUS-BEREICH                       *  
      *            (JEWEILS SEPARATE COPYS)                          *  
      ****************************************************************  
      *                                                                 
       01 PI-DRVB0668.                                                  
          05 PI-DRVB0668-MA-ST-NR           PIC S9(009) BINARY.         
          05 PI-DRVB0668-ZEITPUNKT-ISO      PIC  X(010).                
      *                                                                 
       01 PO-DRVB0668.                                                  
                                                                        
          05 PO-DRVB0668-RC                 PIC  X(003).                
             88 PO-DRVB0668-RC-OK                VALUE 'OK '.           
             88 PO-DRVB0668-RC-LEER              VALUE 'LER'.           
             88 PO-DRVB0668-RC-EOF               VALUE 'EOF'.           
             88 PO-DRVB0668-RC-NOT-FOUND         VALUE 'NFD'.           
                                                                        
          05 PO-DRVB0668-DATEN.                                         
             10 PO-DRVB0668-AST-JEQF-GR     PIC  S9(3)V99 COMP-3.       
             10 PO-DRVB0668-AST-JEQF-GS     PIC  S9(3)V99 COMP-3.       
             10 PO-DRVB0668-AST-JEQF-EG     PIC  S9(3)V99 COMP-3.       
      *                                                                 
      ***** ENDE COPY-MEMBER DRVB0668 *****************************     

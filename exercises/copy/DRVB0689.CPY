      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  Januar 2003                               
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRVB0689.CPY  $                                         

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRVB0689.CPv  $                                
      *
      *   Rev 2.1   Nov 27 2003 13:38:28   t13ex57
      *AVB00711 - neue Funktion in ZJVB0015
      *                                                                 
      *   Rev 2.0   Feb 17 2003 11:00:56   p13sp16                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      *    COPY-MEMBER: DRVB0689                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0015                        * 
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: U. BOLL                                          * 
      *    DATUM:    04.11.2003                                       * 
      ***************************************************************** 
      *    VERSION  1                                                   
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AVB00711!04.11.03! 01 ! NEUERSTELLUNG             ! U. BOLL  *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      ****************************************************************  
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *  
      *    FÜR DIE FUNKTION DRVB0689                                 *  
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *  
      *            DEM ERR-ERROR-BEREICH UND                         *  
      *            DEM STANDARD-STATUS-BEREICH                       *  
      *            (JEWEILS SEPARATE COPYS)                          *  
      *                                                              *  
      ****************************************************************  
      *                                                                 
      *                                                                 
       01 PI-DRVB0689.                                                  
          05 PI-DRVB0689-PROGRAMM           PIC  X(008).                
          05 PI-DRVB0689-UFKT-CODE          PIC  9(002).                
          05 PI-DRVB0689-ED-BESITZER        PIC  S9(009) COMP.          
          05 PI-DRVB0689-DAT-VON            PIC  X(010).                
      *                                                                 
       01 PO-DRVB0689.                                                  
                                                                        
          05 PO-DRVB0689-RC                 PIC  X(003).                
             88 PO-DRVB0689-RC-OK                        VALUE 'OK '.   
             88 PO-DRVB0689-RC-LEER                      VALUE 'LER'.   
             88 PO-DRVB0689-RC-EOF                       VALUE 'EOF'.   
             88 PO-DRVB0689-RC-NOT-FOUND                 VALUE 'NFD'.   
                                                                        
          05 PO-DRVB0689-DATEN.                                         
             10 PO-DRVB0689-ED-ST-NR         PIC  S9(009) COMP.         
             10 PO-DRVB0689-ED-BETREUTER     PIC  S9(009) COMP.         
      *                                                                 
      ***** ENDE COPY-MEMBER DRVB0689 *****************************     

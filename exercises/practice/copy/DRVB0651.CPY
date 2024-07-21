      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  Januar 2003                               
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   drvb0651.cpy  $                                 
      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/cpy/DRVB0651.CPv  $                                
      *
      *   Rev 2.1   Apr 22 2003 16:21:56   t13ae35
      *AVB00618 - neu, Jahreseinkommen je VB aus FMIT
      *                                                                 
      *   Rev 2.0   Feb 17 2003 10:57:14   p13sp16                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      *    COPY-MEMBER: DRVB0651                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0087                        * 
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: J.Hering                                         * 
      *    DATUM:    04.04.2003                                       * 
      ***************************************************************** 
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * --------+--------+----+---------------------------+----------*  
      * AVB00618!04.04.03! 2.1! neu, Jahreseinkommen je VB! J.Hering *  
      * --------+--------+----+---------------------------+----------*  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *  
      *    FÜR DIE FUNKTION DRVB0651                                 *  
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *  
      *            DEM ERR-ERROR-BEREICH UND                         *  
      *            DEM STANDARD-STATUS-BEREICH                       *  
      *            (JEWEILS SEPARATE COPYS)                          *  
      *                                                              *  
      ****************************************************************  
      *                                                                 
       01 PI-DRVB0651.                                                  
          05 PI-DRVB0651-UFKT-CODE          PIC  9(002).                
          05 PI-DRVB0651-JAHR-JJJJ          PIC  S9(004)      BINARY.   
          05 PI-DRVB0651-BETRAG             PIC  S9(09)V9(02) COMP-3.   
      *                                                                 
       01 PO-DRVB0651.                                                  
                                                                        
          05 PO-DRVB0651-RC                 PIC  X(003).                
             88 PO-DRVB0651-RC-OK                        VALUE 'OK '.   
             88 PO-DRVB0651-RC-LEER                      VALUE 'LER'.   
             88 PO-DRVB0651-RC-EOF                       VALUE 'EOF'.   
             88 PO-DRVB0651-RC-NOT-FOUND                 VALUE 'NFD'.   
                                                                        
          05 PO-DRVB0651-DATEN.                                         
             10 PO-DRVB0651-MA-ST-NR        PIC S9(009)      BINARY.    
             10 PO-DRVB0651-SUMME-BETRAG    PIC S9(09)V9(02) COMP-3.    
      *                                                                 
      ***** ENDE COPY-MEMBER DRVB0651 *****************************     

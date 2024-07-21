      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  Januar 1999                               
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright 1999 ATLAS Dienstleistungs GmbH                    
      *---------------------------------------------------------------* 
      *    $Workfile:   drvb0467.cpy  $                                 
      *    $Revision:   2.3  $                                          
      *                                                                 
      *    $Log:   L:/MF/CPY/DRVB0467.CPv  $                            
      *
      *   Rev 2.3   Mar 31 2003 12:53:10   t13ae35
      *AVB00666 - Korrektur FÖBO-I läuft aus
      *                                                                 
      *   Rev 2.1   Apr 18 2000 12:43:04   T13AE26                      
      *ASL00046 Einführung Förderbonus (Dimension 7)                    
      *                                                                 
      *   Rev 2.0   Apr 03 2000 14:10:50   P13SP20                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      *    COPY-MEMBER: DRVB0467                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0018                        * 
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: T. Moll                                          * 
      *    DATUM:    30.03.2000                                       * 
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * ASL00046!30.03.00!2.1 ! NEUERSTELLUNG             ! T. Moll  *  
      * -------------------------------------------------------------*  
070303* AVB00666!07.03.03!2.2 ! Korrektur FÖBO-I läuft aus!          *  
      *         !        !    ! VBA-Bonusstufenproblematik! J.Hering *  
      * --------+--------+----+---------------------------+----------*  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *  
      *    FÜR DIE FUNKTION DRVB0467:                                *  
      *     LESEN ALLER NEUEN AL, DIE IM ZEITRAUM IHRE PROBEZEIT     *  
      *     (ALS AL-P) ERFOLGREICH üBERSTANDEN HABEN (FüR FöRDERBONUS)  
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *  
      *            DEM ERR-ERROR-BEREICH UND                         *  
      *            DEM STANDARD-STATUS-BEREICH                       *  
      *            (JEWEILS SEPARATE COPYS)                          *  
      *                                                              *  
      ****************************************************************  
      *                                                                 
      *                                                                 
       01 PI-DRVB0467.                                                  
          05 PI-DRVB0467-PROGRAMM           PIC  X(008).                
          05 PI-DRVB0467-UFKT-CODE          PIC  9(002).                
          05 PI-DRVB0467-ERMITT-VON-ISO     PIC  X(010).                
          05 PI-DRVB0467-ERMITT-BIS-ISO     PIC  X(010).                
          05 PI-DRVB0467-ALP-ERSTMAL-ISO    PIC  X(010).                
      *                                                                 
       01 PO-DRVB0467.                                                  
                                                                        
          05 PO-DRVB0467-RC                 PIC  X(003).                
             88 PO-DRVB0467-RC-OK                        VALUE 'OK '.   
             88 PO-DRVB0467-RC-LEER                      VALUE 'LER'.   
             88 PO-DRVB0467-RC-EOF                       VALUE 'EOF'.   
             88 PO-DRVB0467-RC-NOT-FOUND                 VALUE 'NFD'.   
                                                                        
          05 PO-DRVB0467-DATEN.                                         
             10 PO-DRVB0467-MA-ST-NR        PIC  S9(009) BINARY.        
             10 PO-DRVB0467-ALP-BIS-ISO     PIC   X(010).               
             10 PO-DRVB0467-AL-BESTAET-ISO  PIC   X(010).               
070303       10 PO-DRVB0467-ALP-SEIT-ISO    PIC   X(010).               
      *                                                                 
      ***** ENDE COPY-MEMBER DRVB0467 *****************************     

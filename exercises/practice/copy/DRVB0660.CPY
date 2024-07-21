      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  Januar 2003                               
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRVB0660.CPY  $                                         

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/cpy/DRVB0660.CPv  $                                
      *
      *   Rev 2.1   May 30 2003 10:45:52   t13ae35
      *ACV00016 - neu
      *                                                                 
      *   Rev 2.0   Feb 17 2003 10:58:06   p13sp16                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      *    COPY-MEMBER: DRVB0660                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0049                        * 
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        * 
      *    --------------------------------------------------------   * 
      *    Beschreibung: zu VBNr wird der Austrittsgrundes des        * 
      *                  jüngsten (last-update) Satzes in der MAT gel.* 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: Hering                                           * 
      *    DATUM:    22.05.2003                                       * 
      ***************************************************************** 
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * ACV00016!22.05.03! 2.1! NEU                       ! Hering   *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *  
      *    FÜR DIE FUNKTION DRVB0660                                 *  
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *  
      *            DEM ERR-ERROR-BEREICH UND                         *  
      *            DEM STANDARD-STATUS-BEREICH                       *  
      *            (JEWEILS SEPARATE COPYS)                          *  
      *                                                              *  
      ****************************************************************  
      *                                                                 
      *                                                                 
       01 PI-DRVB0660.                                                  
          05 PI-DRVB0660-MA-ST-NR           PIC S9(009) COMP.           
      *                                                                 
       01 PO-DRVB0660.                                                  
                                                                        
          05 PO-DRVB0660-RC                 PIC  X(003).                
             88 PO-DRVB0660-RC-OK                        VALUE 'OK '.   
             88 PO-DRVB0660-RC-LEER                      VALUE 'LER'.   
             88 PO-DRVB0660-RC-EOF                       VALUE 'EOF'.   
             88 PO-DRVB0660-RC-NOT-FOUND                 VALUE 'NFD'.   
                                                                        
          05 PO-DRVB0660-DATEN.                                         
             10 PO-DRVB0660-MAT-AUS-GRND     PIC S9(004) COMP.          
      *                                                                 
      ***** ENDE COPY-MEMBER DRVB0660 *****************************     

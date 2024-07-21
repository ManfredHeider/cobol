      *---------------------------------------------------------------*         
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS                                 
      *            DATUM   :  NOVEMBER 1995                                     
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH                            
      *---------------------------------------------------------------*         
      *    $WORKFILE:   DRVB0269.CPY  $                                         
      *    $REVISION:   1.1  $                                                  
      *                                                                         
      *    $LOG:   L:/MF/CPY/DRVB0269.CPV  $                                    
      *                                                                         
      *   REV 1.1   17 JUN 1998 15:47:26   T13EX30                              
      *AAS00049 - NEUERSTELLUNG FUER DELTA-RUECKKONVERTIERUNG                   
      *                                                                         
      *   REV 1.0   23 JUL 1997 12:24:26   P13SP99                              
      *INITIAL REVISION.                                                        
      *                                                                         
      *---------------------------------------------------------------*         
      *---------------------------------------------------------------*         
      *    COPY-MEMBER: DRVB0269                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0007                        *         
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: WALB                                             *         
      *    DATUM:    16.06.1998                                       *         
      *****************************************************************         
      *    VERSION  1                                                           
      ****************************************************************          
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
      * -------------------------------------------------------------*          
      * AAS00049!16.06.98! 01 ! NEUERSTELLUNG             ! WALB     *          
      *         !        !    !                           !          *          
      *         !        !    !                           !          *          
      *                                                              *          
      * ENDE-LOGBUCH                                                 *          
      ****************************************************************          
      ****************************************************************          
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *          
      *    FÜR DIE FUNKTION DRVB0269                                 *          
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *          
      *            DEM ERR-ERROR-BEREICH UND                         *          
      *            DEM STANDARD-STATUS-BEREICH                       *          
      *            (JEWEILS SEPARATE COPYS)                          *          
      *                                                              *          
      ****************************************************************          
      *                                                                         
      *                                                                         
       01 PI-DRVB0269.                                                          
          05 PI-DRVB0269-MA-ST-NR           PIC S9(009) COMP.                   
      *                                                                         
       01 PO-DRVB0269.                                                          
                                                                                
          05 PO-DRVB0269-RC                 PIC  X(003).                        
             88 PO-DRVB0269-RC-OK                        VALUE 'OK '.           
             88 PO-DRVB0269-RC-LEER                      VALUE 'LER'.           
             88 PO-DRVB0269-RC-EOF                       VALUE 'EOF'.           
             88 PO-DRVB0269-RC-NOT-FOUND                 VALUE 'NFD'.           
                                                                                
          05 PO-DRVB0269-DATEN.                                                 
             10 PO-DRVB0269-VBP-NR           PIC  S9(004) COMP.                 
      *                                                                         
      ***** ENDE COPY-MEMBER DRVB0269 *****************************             
      *
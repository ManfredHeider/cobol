      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  Januar 2003                               
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   drvb0641.cpy  $                                 
      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/cpy/DRVB0641.CPv  $                                
      *
      *   Rev 2.1   Mar 31 2003 12:53:44   t13ae35
      *AVB00666 - neu
      *                                                                 
      *   Rev 2.0   Feb 17 2003 10:56:18   p13sp16                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      *    COPY-MEMBER: DRVB0641                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0013 (PI/PO)                * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: Hering                                           * 
      *    DATUM:    07.03.2003                                       * 
      ***************************************************************** 
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AVB00666!07.03.03! 2.1! NEUERSTELLUNG             ! Hering   *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *  
      *    FÜR DIE FUNKTION DRVB0641                                 *  
      *    Zu VB-Nr und Stichtag wird das Datum der VM-Vereinbarung  *  
      *    (DAT-VON) geliefert des DAT-BIS gleich dem Stichtag ist.  *  
      ****************************************************************  
      *                                                                 
       01 PI-DRVB0641.                                                  
          05 PI-DRVB0641-MA-ST-NR           PIC S9(009) COMP.           
          05 PI-DRVB0641-STICHTAG-ISO       PIC  X(010).                
      *                                                                 
       01 PO-DRVB0641.                                                  
                                                                        
          05 PO-DRVB0641-RC                 PIC  X(003).                
             88 PO-DRVB0641-RC-OK                        VALUE 'OK '.   
             88 PO-DRVB0641-RC-LEER                      VALUE 'LER'.   
             88 PO-DRVB0641-RC-EOF                       VALUE 'EOF'.   
             88 PO-DRVB0641-RC-NOT-FOUND                 VALUE 'NFD'.   
                                                                        
          05 PO-DRVB0641-DATEN.                                         
             10 PO-DRVB0641-AV-DAT-VON       PIC  X(010).               
      *                                                                 
      ***** ENDE COPY-MEMBER DRVB0641 *****************************     

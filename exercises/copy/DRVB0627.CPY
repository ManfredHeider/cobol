      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  November 2002                                     
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DRVB0627.CPY  $                                         
      *    $Revision:   2.2  $                                                  
      *                                                                         
      *    $Log:   L:/mf/cpy/DRVB0627.CPv  $
      *
      *   Rev 2.2   Jan 02 2003 17:25:06   t13ae35
      *ASL00153 - CR/LF in Spalte 83 entfernt
      *
      *   Rev 2.1   Jan 02 2003 15:28:46   t13ae35
      *ASL00153 - neu
      *                                                                         
      *   Rev 2.0   Dec 04 2002 12:21:14   t13ex15                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *    COPY-MEMBER: DRVB0627                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0088                        *         
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *         
      *                                                               *         
      *    gesamtes Tupel aus BOZA                                    *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: J.Hering                                         *         
      *    DATUM:    11.12.2002                                       *         
      *****************************************************************         
      *    VERSION  1                                                           
      ****************************************************************          
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
      * --------+--------+----+---------------------------+----------*          
      * ASL00153!11.12.02! 2.1! Neu, gesamtes Tupel       ! Hering   *          
      * --------+--------+----+---------------------------+----------*          
      *                                                              *          
      * ENDE-LOGBUCH                                                 *          
      ****************************************************************          
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *          
      *    FÜR DIE FUNKTION DRVB0627                                 *          
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *          
      *            DEM ERR-ERROR-BEREICH UND                         *          
      *            DEM STANDARD-STATUS-BEREICH                       *          
      *            (JEWEILS SEPARATE COPYS)                          *          
      ****************************************************************          
      *                                                                         
       01 PI-DRVB0627.                                                          
          05 PI-DRVB0627-BOZA-ST-NR         PIC S9(009) BINARY.                 
          05 PI-DRVB0627-BOZA-ANSPR-VOM     PIC  X(010).                        
          05 PI-DRVB0627-KNR-ANSPR-GRUND    PIC S9(004) BINARY.                 
      *                                                                         
       01 PO-DRVB0627.                                                          
                                                                                
          05 PO-DRVB0627-RC                 PIC  X(003).                        
             88 PO-DRVB0627-RC-OK                VALUE 'OK '.                   
             88 PO-DRVB0627-RC-LEER              VALUE 'LER'.                   
             88 PO-DRVB0627-RC-EOF               VALUE 'EOF'.                   
             88 PO-DRVB0627-RC-NOT-FOUND         VALUE 'NFD'.                   
                                                                                
          05 PO-DRVB0627-DATEN.                                                 
             10 PO-DRVB0627-BOZA-TUPEL-GESAMT PIC  X(078).                      
      *                                                                         
      ***** ENDE COPY-MEMBER DRVB0627 *****************************             

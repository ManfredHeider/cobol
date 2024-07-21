      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  Mai 2002                                          
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH                        
      *---------------------------------------------------------------*         
      *    $Workfile:   DRVB0616.CPY  $                                         
      *    $Revision:   2.2  $                                                  
      *                                                                         
      *    $Log:   L:/mf/cpy/DRVB0616.CPv  $
      *
      *   Rev 2.2   Dec 10 2002 16:16:14   t13ae35
      *ASL00153 - Zeichen in Spalte 84 entfernt
      *
      *   Rev 2.1   Dec 10 2002 15:25:16   t13ae35
      *ASL00153 - neu, alle Beförderungen zu gegebener Stufe
      *im Zeitraum
      *                                                                         
      *   Rev 2.0   Nov 05 2002 11:36:22   p13sp16                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *    COPY-MEMBER: DRVB0616                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0012                        *         
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERBEBEN WERDEN        *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: J. Hering                                        *         
      *    DATUM:    27.11.2002                                       *         
      ****************************************************************          
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
      * -------------------------------------------------------------*          
      * ASL00153!27.11.02!2.1 ! NEUERSTELLUNG             ! J. Hering*          
      *         !        !    !                           !          *          
      *         !        !    !                           !          *          
      *         !        !    !                           !          *          
      *                                                              *          
      * ENDE-LOGBUCH                                                 *          
      ****************************************************************          
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *          
      *    alle Beförderungen (bzw. Direkteinsteiger) zu einer       *          
      *    gegebenen Stufe in einem gegebenen Zeitraum               *          
      ****************************************************************          
      *                                                                         
      *                                                                         
       01 PI-DRVB0616.                                                          
          05 PI-DRVB0616-PROGRAMM           PIC  X(008).                        
          05 PI-DRVB0616-UFKT-CODE          PIC  9(002).                        
          05 PI-DRVB0616-ZEITRAUM-VON-ISO   PIC  X(010).                        
          05 PI-DRVB0616-ZEITRAUM-BIS-ISO   PIC  X(010).                        
          05 PI-DRVB0616-MS-NR              PIC S9(004) BINARY.                 
                                                                                
      *                                                                         
       01 PO-DRVB0616.                                                          
                                                                                
          05 PO-DRVB0616-RC                 PIC  X(003).                        
             88 PO-DRVB0616-RC-OK                        VALUE 'OK '.           
             88 PO-DRVB0616-RC-LEER                      VALUE 'LER'.           
             88 PO-DRVB0616-RC-EOF                       VALUE 'EOF'.           
             88 PO-DRVB0616-RC-NOT-FOUND                 VALUE 'NFD'.           
                                                                                
          05 PO-DRVB0616-DATEN.                                                 
             10 PO-DRVB0616-MA-ST-NR        PIC S9(009) BINARY.                 
             10 PO-DRVB0616-MS-NR           PIC S9(004) BINARY.                 
             10 PO-DRVB0616-MS-BEZ-KURZ     PIC  X(004).                        
             10 PO-DRVB0616-DAT-VON-ISO     PIC  X(010).                        
             10 PO-DRVB0616-NAME-VORNAME    PIC  X(040).                        
             10 PO-DRVB0616-DIR-NR          PIC S9(004) BINARY.                 
             10 PO-DRVB0616-GESELL-TYP      PIC S9(004) BINARY.                 
             10 PO-DRVB0616-PRUEFZIFFER     PIC  X(001).                        
      *                                                                         
      ***** ENDE COPY-MEMBER DRVB0616 *****************************             

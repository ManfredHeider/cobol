      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  November 1995                                     
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright 1995 ATLAS Dienstleistungs GmbH                            
      *---------------------------------------------------------------*         
      *    $Workfile:   DRVB0427.CPY  $                                         
      *    $Revision:   2.2  $                                                  
      *                                                                         
      *    $Log:   L:/MF/CPY/DRVB0427.CPv  $
      *
      *   Rev 2.2   Dec 10 2002 16:15:48   t13ae35
      *ASL00153 - Zeichen in Spalte 84 entfernt
      *
      *   Rev 2.1   Dec 10 2002 15:21:28   t13ae35
      *ASL00153 -neu, alle VB zum Stuchtag ab geg. Stufe
      *                                                                         
      *   Rev 2.0   Jun 02 1999 14:44:58   P13SP20                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *    COPY-MEMBER: DRVB0427                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0012                        *         
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *         
      *    Ermittlung aller VB zum Stichtag ab gegebener Stufe        *         
      *    bis gegebenr Stufe                                         *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: J.Hering                                         *         
      *    DATUM:    27.11.2002                                       *         
      *****************************************************************         
      *    VERSION  1                                                           
      ****************************************************************          
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
      * --------+--------+----+---------------------------+----------*          
      * ASL00153!27.11.02! 2.1! Neu, alle VB zum Stichtag ! Hering   *          
      *         !        !    ! ab einer gegebenen Stufe  !          *          
      * --------+--------+----+---------------------------+----------*          
      *                                                              *          
      * ENDE-LOGBUCH                                                 *          
      ****************************************************************          
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *          
      *    FÜR DIE FUNKTION DRVB0427                                 *          
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *          
      *            DEM ERR-ERROR-BEREICH UND                         *          
      *            DEM STANDARD-STATUS-BEREICH                       *          
      *            (JEWEILS SEPARATE COPYS)                          *          
      ****************************************************************          
      *                                                                         
       01 PI-DRVB0427.                                                          
          05 PI-DRVB0427-PROGRAMM           PIC  X(008).                        
          05 PI-DRVB0427-UFKT-CODE          PIC  9(002).                        
          05 PI-DRVB0427-MS-NR-VON          PIC S9(004) BINARY.                 
          05 PI-DRVB0427-MS-NR-BIS          PIC S9(004) BINARY.                 
          05 PI-DRVB0427-ZEITPUNKT-ISO      PIC  X(010).                        
      *                                                                         
       01 PO-DRVB0427.                                                          
                                                                                
          05 PO-DRVB0427-RC                 PIC  X(003).                        
             88 PO-DRVB0427-RC-OK                        VALUE 'OK '.           
             88 PO-DRVB0427-RC-LEER                      VALUE 'LER'.           
             88 PO-DRVB0427-RC-EOF                       VALUE 'EOF'.           
             88 PO-DRVB0427-RC-NOT-FOUND                 VALUE 'NFD'.           
                                                                                
          05 PO-DRVB0427-DATEN.                                                 
             10 PO-DRVB0427-MA-ST-NR        PIC S9(009) BINARY.                 
             10 PO-DRVB0427-MS-NR           PIC S9(004) BINARY.                 
             10 PO-DRVB0427-MS-BEZ-KURZ     PIC  X(004).                        
             10 PO-DRVB0427-DAT-VON-ISO     PIC  X(010).                        
             10 PO-DRVB0427-NAME-VORNAME    PIC  X(040).                        
             10 PO-DRVB0427-DIR-NR          PIC S9(004) BINARY.                 
             10 PO-DRVB0427-GESELL-TYP      PIC S9(004) BINARY.                 
             10 PO-DRVB0427-PRUEFZIFFER     PIC  X(001).                        
      *                                                                         
      ***** ENDE COPY-MEMBER DRVB0427 *****************************             

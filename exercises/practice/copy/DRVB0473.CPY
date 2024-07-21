      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  Januar 1999                                       
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright 1999 ATLAS Dienstleistungs GmbH                            
      *---------------------------------------------------------------*         
      *    $Workfile:   DRVB0473.CPY  $                                         
      *    $Revision:   2.2  $                                                  
      *                                                                         
      *    $Log:   L:/MF/CPY/DRVB0473.CPv  $                                    
      *
      *   Rev 2.2   Nov 27 2002 09:33:26   t13ae35
      *ASL00152 - FB-Stufen als neues PI-Feld (1000-EUR-GSH)
      *                                                                         
      *   Rev 2.1   09 May 2000 18:18:10   T13AE30                              
      *ASL00046 - Förderbonus Auszahlung                                        
      *                                                                         
      *   Rev 2.0   Apr 03 2000 14:11:00   P13SP20                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *---------------------------------------------------------------*         
      *    COPY-MEMBER: DRVB0473                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0077                        *         
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: Walb                                             *         
      *    DATUM:    28.04.2000                                       *         
      *****************************************************************         
      *    VERSION  1                                                           
      ****************************************************************          
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
      * --------+--------+----+---------------------------+----------*          
      * ASL00046!28.04.00! 2.1! NEUERSTELLUNG             ! Walb     *          
      * --------+--------+----+---------------------------+----------*          
      * ASL00152!25.11.02! 2.2! FB-Stufe als neue PI-Feld.! Hering   *          
      *         !        !    !                           !          *          
      *                                                              *          
      * ENDE-LOGBUCH                                                 *          
      ****************************************************************          
      ****************************************************************          
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *          
      *    FÜR DIE FUNKTION DRVB0473                                 *          
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *          
      *            DEM ERR-ERROR-BEREICH UND                         *          
      *            DEM STANDARD-STATUS-BEREICH                       *          
      *            (JEWEILS SEPARATE COPYS)                          *          
      *                                                              *          
      ****************************************************************          
      *                                                                         
       01 PI-DRVB0473.                                                          
          05 PI-DRVB0473-UFKT-CODE          PIC  9(002).                        
          05 PI-DRVB0473-MONAT              PIC  9(002).                        
          05 PI-DRVB0473-JAHR               PIC  9(004).                        
          05 PI-DRVB0473-FB-STUFE-VON       PIC S9(004) BINARY.                 
          05 PI-DRVB0473-FB-STUFE-BIS       PIC S9(004) BINARY.                 
      *                                                                         
       01 PO-DRVB0473.                                                          
                                                                                
          05 PO-DRVB0473-RC                 PIC  X(003).                        
             88 PO-DRVB0473-RC-OK                        VALUE 'OK '.           
             88 PO-DRVB0473-RC-LEER                      VALUE 'LER'.           
             88 PO-DRVB0473-RC-EOF                       VALUE 'EOF'.           
             88 PO-DRVB0473-RC-NOT-FOUND                 VALUE 'NFD'.           
                                                                                
          05 PO-DRVB0473-DATEN.                                                 
             10 PO-DRVB0473-ST-NR            PIC S9(009)  COMP.                 
             10 PO-DRVB0473-KNR-WAEHRUNG     PIC S9(004)  COMP.                 
             10 PO-DRVB0473-KTY-WAEHRUNG     PIC S9(004)  COMP.                 
             10 PO-DRVB0473-FBAZ-BETRAG      PIC 9(011).                        
             10 PO-DRVB0473-UNTST-NR         PIC S9(009)  COMP.                 
             10 PO-DRVB0473-KNR-ANSPR-FBONUS PIC S9(009)  COMP.                 
             10 PO-DRVB0473-FBAZ-ANSPR-VOM   PIC X(010).                        
      *                                                                         
      ***** ENDE COPY-MEMBER DRVB0473 *****************************             

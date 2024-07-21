      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  November 2002                                     
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DRVB0625.CPY  $                                         
      *    $Revision:   2.2  $                                                  
      *                                                                         
      *    $Log:   L:/mf/cpy/DRVB0625.CPv  $
      *
      *   Rev 2.2   Jan 02 2003 17:24:48   t13ae35
      *ASL00153 - CR/LF in Spalte 83 entfernt
      *
      *   Rev 2.1   Jan 02 2003 15:27:32   t13ae35
      *ASL00153 - neu
      *                                                                         
      *   Rev 2.0   Dec 04 2002 12:21:04   t13ex15                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *    COPY-MEMBER: DRVB0625                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0088                        *         
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: J.Hering                                         *         
      *    DATUM:    11.12.2002                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
      * --------+--------+----+---------------------------+----------*          
      * ASL00153!11.12.02! 2.1! neu, BOZ-Ansprüche        ! J.Hering *          
      *         !        !    ! je Monat                  !          *          
      * --------+--------+----+---------------------------+----------*          
      *                                                              *          
      * ENDE-LOGBUCH                                                 *          
      ****************************************************************          
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *          
      *    FÜR DIE FUNKTION DRVB0625                                 *          
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *          
      *            DEM ERR-ERROR-BEREICH UND                         *          
      *            DEM STANDARD-STATUS-BEREICH                       *          
      *            (JEWEILS SEPARATE COPYS)                          *          
      *                                                              *          
      ****************************************************************          
      *                                                                         
       01 PI-DRVB0625.                                                          
          05 PI-DRVB0625-UFKT-CODE          PIC  9(002).                        
          05 PI-DRVB0625-ANSPR-VOM          PIC  X(010).                        
          05 PI-DRVB0625-ANSPR-STATUS       PIC  S9(004) BINARY.                
      *                                                                         
       01 PO-DRVB0625.                                                          
                                                                                
          05 PO-DRVB0625-RC                 PIC  X(003).                        
             88 PO-DRVB0625-RC-OK                        VALUE 'OK '.           
             88 PO-DRVB0625-RC-LEER                      VALUE 'LER'.           
             88 PO-DRVB0625-RC-EOF                       VALUE 'EOF'.           
             88 PO-DRVB0625-RC-NOT-FOUND                 VALUE 'NFD'.           
                                                                                
          05 PO-DRVB0625-DATEN.                                                 
             10 PO-DRVB0625-BOZA-ST-NR         PIC S9(009) BINARY.              
             10 PO-DRVB0625-BOZA-ANSPR-VOM     PIC  X(010).                     
             10 PO-DRVB0625-KNR-ANSPR-GRUND    PIC S9(004) BINARY.              
             10 PO-DRVB0625-BOZA-ANSPR-BETRAG  PIC S9(09)V9(02) COMP-3.         
      *                                                                         
      ***** ENDE COPY-MEMBER DRVB0625 *****************************             

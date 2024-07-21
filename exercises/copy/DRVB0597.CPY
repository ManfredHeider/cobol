000010*---------------------------------------------------------------*         
000020*    Standard für alle Objekte unter PVCS                                 
000030*            Datum   :  Januar 2001                                       
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
000050*---------------------------------------------------------------*         
000060*                                                                         
000070*---------------------------------------------------------------*         
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH                            
000090*---------------------------------------------------------------*         
000100*    $Workfile:   DRVB0597.CPY  $                                         
000110*    $Revision:   2.2  $                                                  
000120*                                                                         
000130*    $Log:   L:/mf/cpy/DRVB0597.CPv  $
000140*
000150*   Rev 2.2   Nov 05 2002 17:04:22   t13ex57
000160*AVB00636 - Zeichen ausserhalb der ersten 80 Bytes
000170*
000180*   Rev 2.1   Nov 05 2002 16:27:54   t13ex57
000190*AVB00636 - Erweiterung Email/URL SST
000200*                                                                         
000210*   Rev 2.0   Feb 25 2002 17:33:52   p13sp20                              
000220*Archiverstellung                                                         
000230*                                                                         
000240*---------------------------------------------------------------*         
000250*    COPY-MEMBER: DRVB0597                                      *         
000260*    --------------------------------------------------------   *         
000270*    SCHNITTSTELLE FÜR DAS UPRO ZWVB0019                        *         
000280*    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *         
000290*    --------------------------------------------------------   *         
000300*    ERSTELLT: U. BOLL                                          *         
000310*    DATUM:    16.10.2002                                       *         
000320*****************************************************************         
000330*    VERSION  1                                                           
000340****************************************************************          
000350* AENDERUNGEN:                                                 *          
000360* AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
000370* -------------------------------------------------------------*          
000380* AVB00636!16.10.02! 01 ! NEUERSTELLUNG             ! U. BOLL  *          
000390*         !        !    !                           !          *          
000400*         !        !    !                           !          *          
000410*                                                              *          
000420* ENDE-LOGBUCH                                                 *          
000430****************************************************************          
000440****************************************************************          
000450*    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *          
000460*    FÜR DIE FUNKTION DRVB0597                                 *          
000470*    AUSSER: DEM FUNKTIONSNAMEN,                               *          
000480*            DEM ERR-ERROR-BEREICH UND                         *          
000490*            DEM STANDARD-STATUS-BEREICH                       *          
000500*            (JEWEILS SEPARATE COPYS)                          *          
000510*                                                              *          
000520****************************************************************          
000530*                                                                         
000540*                                                                         
000550 01 PI-DRVB0597.                                                          
000560    05 PI-DRVB0597-DIR-NR             PIC S9(004) COMP.                   
000570    05 PI-DRVB0597-DL-BEZUGS-NR       PIC S9(009) COMP.                   
000580*                                                                         
000590 01 PO-DRVB0597.                                                          
000600    05 PO-DRVB0597-RC                 PIC  X(003).                        
000610       88 PO-DRVB0597-RC-OK                        VALUE 'OK '.           
000620       88 PO-DRVB0597-RC-LEER                      VALUE 'LER'.           
000630       88 PO-DRVB0597-RC-EOF                       VALUE 'EOF'.           
000640       88 PO-DRVB0597-RC-NOT-FOUND                 VALUE 'NFD'.           
000650                                                                          
000660    05 PO-DRVB0597-DATEN.                                                 
000670       10 PO-DRVB0597-DATUM           PIC  X(026).                        
000680*                                                                         
000690***** Ende Copy-Member DRVB0597 *****************************             

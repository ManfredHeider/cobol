000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2002
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2002 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   test00.CPY  $
000110*    $Revision:   1.6  $
000120*
000130*    $Log:   L:/MF/CPY/test00.CPv  $
      *
      *   Rev 1.6   Nov 06 2002 10:34:30   p13sp16
      *test
      *
      *    $Header:   L:/MF/CPY/test00.CPv   1.6   Nov 06 2002 10:34:30   p13sp16  $
      *
      *
      *   Rev 1.5   Nov 06 2002 09:51:34   p13sp16
      *einmal im MFE geladen und gespeichert
      *
      *   Rev 1.4   Nov 06 2002 09:37:52   p13sp16
      *test
      *
      *   Rev 1.3   Nov 06 2002 09:35:38   p13sp16
      *leerzeichen gelöscht
      *
      *   Rev 1.2   Nov 06 2002 09:25:04   p13sp16
      *test wg. line too long
000140*
000150*   Rev 2.2   Nov 05 2002 17:05:04   t13ex57
000160*AVB00636 - Zeichen ausserhalb der ersten 80 Bytes
000170*
000180*   Rev 2.1   Nov 05 2002 16:29:10   t13ex57
000190*AVB00636 - Erweiterung Email/URL SST
000200*
000210*   Rev 2.0   Feb 25 2002 17:34:10   p13sp20                              
000220*Archiverstellung                                                         
000230*
000240*---------------------------------------------------------------*
000250*****************************************************************
000260*    COPY-MEMBER: DRVB0600                                      *
000270*    --------------------------------------------------------   *
000280*    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0600                    *
000290*    --------------------------------------------------------   *
000300*    ERSTELLT: U. BOLL                                          *
000310*    DATUM:    18.10.2002                                       *
000320*****************************************************************
000330*    VERSION
000340****************************************************************
000350* AENDERUNGEN:                                                 *          
000360* AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
000370* -------------------------------------------------------------*          
000380* AVB00636!18.10.02!    ! NEU                       !U.BOLL    *          
000390*         !        !    !                           !          *          
000400*         !        !    !                           !          *          
000410*                                                              *          
000420* ENDE-LOGBUCH                                                 *          
000430****************************************************************          
000440*                                                                         
000450*    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER                    
000460*    FÜR DIE FUNKTION DRVB0600                                            
000470*    AUSSER: DEM FUNKTIONSNAMEN,                                          
000480*            DEM ERR-ERROR-BEREICH UND                                    
000490*            DEM STANDARD-STATUS-BEREICH                                  
000500*            (JEWEILS SEPARATE COPYS)                                     
000510*                                                                         
000520*    >>>  SELECT ...                <<<                                   
000530*                                                                         
000540****************************************************************          
000550*                                                                         
000560 01  PI-DRVB0600.                                                         
000570     05  PI-DRVB0600-MA-ST-NR           PIC S9(009) COMP.                 
000580     05  PI-DRVB0600-MS-NR              PIC S9(004) COMP.                 
000590*                                                                         
000600 01  PO-DRVB0600.                                                         
000610     05  PO-DRVB0600-RC                 PIC  X(003).                      
000620         88 PO-DRVB0600-RC-OK                  VALUE 'OK '.               
000630         88 PO-DRVB0600-RC-NOK                 VALUE 'NOK'.               
000640         88 PO-DRVB0600-RC-NOT-FOUND           VALUE 'NFD'.               
000650         88 PO-DRVB0600-RC-ERR                 VALUE 'ERR'.               
000660*                                                                         
000670     05  PO-DRVB0600-MSBZ-DAT-VON       PIC X(026).                       
000680*********************** ENDE COPY-MEMBER *************************        
000690                                                                          
000700                                                                          
000710                                                                          
000720                                                                          
000730                                                                          
000740                                                                          
000750                                                                          
000760                                                                          

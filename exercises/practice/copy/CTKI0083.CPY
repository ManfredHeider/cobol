000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   CTKI0083.CPY  $
000110*    $Revision:   2.2  $
000120*
000130*    $Log:   L:/mf/cpy/CTKI0083.CPv  $
000140*
000150*   Rev 2.2   Jun 10 2002 17:31:50   T13ex48
000160*AKI00274
000170*
000180*   Rev 2.1   May 08 2002 09:51:04   T13ex48
000190*AKI00274 neue Tabelle RDTA DB24
000200*
000210*   Rev 2.0   May 07 2002 15:44:52   p13sp16
000220*Archiverstellung
000230*
000240*---------------------------------------------------------------*

000260******************************************************************
000270* DCLGEN TABLE                                                   *
000280******************************************************************
CCC005*    EXEC SQL DECLARE RDTADB24  TABLE                                     
CCC004*    ( RDTADB24VBID          CHAR(13)   NOT NULL,                         
CCC004*      RDTADB24ERSTELLT      TIMESTAMP  NOT NULL,                         
CCC004*      RDTADB24HERKUNFT      CHAR(02)   NOT NULL,                         
CCC004*      RDTADB24ERL_KZ        CHAR(01)   NOT NULL,                         
CCC004*      RDTADB24ERL_DAT       DATE
CCC003*    ) END-EXEC.                                                          
000360******************************************************************        
000370* COBOL DECLARATION FOR TABLE                                    *        
000380******************************************************************        
000390 01  RDTADB24-GESAMT.                                                     
000400  05  NI-RDTADB24-TAB.                                                    
000410     10 NI-RDTADB24-ELE PIC S9(04) COMP OCCURS 5 TIMES.                   
000420  05  NI-RDTADB24 REDEFINES NI-RDTADB24-TAB.                              
000430     10 NI-RDTADB24VBID             PIC S9(4) COMP.                       
000440     10 NI-RDTADB24ERSTELLT         PIC S9(4) COMP.                       
000450     10 NI-RDTADB24HERKUNFT         PIC S9(4) COMP.                       
000460     10 NI-RDTADB24ERL-KZ           PIC S9(4) COMP.                       
000470     10 NI-RDTADB24ERL-DAT          PIC S9(4) COMP.                       
000480  05  DCL-RDTADB24.                                                       
000490     10 RDTADB24VBID                PIC X(13).                            
000500     10 RDTADB24ERSTELLT            PIC X(26).                            
000510     10 RDTADB24HERKUNFT            PIC X(02).                            
000520     10 RDTADB24ERL-KZ              PIC X(01).                            
000530     10 RDTADB24ERL-DAT             PIC X(10).                            
000540******************************************************************        
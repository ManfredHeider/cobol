000010*---------------------------------------------------------------* 
000020*    Standard für alle Objekte unter PVCS                         
000030*            Datum   :  Januar 2001                               
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
000050*---------------------------------------------------------------* 
000060*                                                                 
000070*---------------------------------------------------------------* 
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH                    
000090*---------------------------------------------------------------* 
000100*    $Workfile:   drvb0598.cpy  $                                 
000110*    $Revision:   2.1  $                                          
000120*                                                                 
000130*    $Log:   L:/mf/cpy/DRVB0598.CPv  $                                
000140*
000150*   Rev 2.1   Apr 22 2003 16:18:20   t13ae35
000160*AVB00618 - neu, Satzbeschreibung Info-Satz (109A ESTG)
000170*                                                                 
000180*   Rev 2.0   Feb 25 2002 17:33:58   p13sp20                      
000190*Archiverstellung                                                 
000200*                                                                 
000210*---------------------------------------------------------------* 
000220****************************************************************  
000230*                                                               * 
000240*   COPY-MEMBER: beschreibt die Satzstruktur des Informations-  * 
000250*                satzes für die österr. Finanzbehörde           * 
000260*---------------------------------------------------------------* 
000270* AENDERUNGEN:                                                 *  
000280* AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
000290* --------+--------+----+---------------------------+----------*  
000300* AVB00618!04.04.03!2.1 ! NEU                       ! J.Hering *  
000310* --------+--------+----+---------------------------+----------*  
000320*         !        !    !                           !          *  
000330****************************************************************  
000340                                                                  
000350 01  DRVB0598-INFOSATZ.                                           
000360*----Satzart                                           -----*01   
000370     05 DRVB0598-SA                            PIC  X(001).       
000380                                                                  
000390*----finanzintern (bleibt leer )                       -----*02   
000400     05 DRVB0598-ADRESSAT                      PIC  X(006).       
000410                                                                  
000420*----finanzintern (bleibt leer )                       -----*03   
000430     05 DRVB0598-DVR-NR1                       PIC  X(007).       
000440                                                                  
000450*----Finanzamtsnummer kein Pflichtfeld (bleibt leer )  -----*04   
000460     05 DRVB0598-FINAMT-NR1.                                      
000470        10 DRVB0598-FINAMT-NR1-NUM             PIC  9(002).       
000480                                                                  
000490*----Steuernummer MS  kein Pflichtfeld (bleibt leer )  -----*05   
000500     05 DRVB0598-STEUER-NR1.                                      
000510        10 DRVB0598-STEUER-NR1-NUM             PIC  9(007).       
000520                                                                  
000530*----DVR-Nummer   MS  kein Pflichtfeld (bleibt leer )  -----*06   
000540     05 DRVB0598-DVR-NR2.                                         
000550        10 DRVB0598-DVR-NR2-NUM                PIC  9(007).       
000560                                                                  
000570*----Finanzamtsnummer AG (konstant )                   -----*07   
000580     05 DRVB0598-FINAMT-NR2                    PIC  9(002).       
000590                                                                  
000600*----Steuernummer AG     (konstant )                   -----*08   
000610     05 DRVB0598-STEUER-NR2                    PIC  9(007).       
000620                                                                  
000630*----DVR-Nummer AG       (konstant )                   -----*09   
000640     05 DRVB0598-DVR-NR3                       PIC  9(007).       
000650                                                                  
000660*----Art der Daten       (konstant )                   -----*10   
000670     05 DRVB0598-ART                           PIC  X(002).       
000680                                                                  
000690*----Datum der Übermittlung (konstant)                 -----*11   
000700     05 DRVB0598-DATUM                         PIC  9(008).       
000710                                                                  
000720*----Uhrzeit der Übermittlung (leer da kein Pflichtf.) -----*12   
000730     05 DRVB0598-UHRZEIT.                                         
000740        10 DRVB0598-UHRZEIT-NUM                PIC  9(006).       
000750                                                                  
000760*----Strukturversion          (konstant)               -----*13   
000770     05 DRVB0598-STRUK-VERSION                 PIC  X(002).       
000780                                                                  
000790*----Name Auftraggeber        (konstant)               -----*14   
000800     05 DRVB0598-AUFGEB-NAME                   PIC  X(037).       
000810                                                                  
000820*----Titel Auftraggeber kein Pflichtfeld (bleibt leer )-----*15   
000830     05 DRVB0598-AUFGEB-TITEL                  PIC  X(020).       
000840                                                                  
000850*----Adresse Auftraggeber     (konstant)               -----*16   
000860     05 DRVB0598-AUFGEB-ADRESSE                PIC  X(037).       
000870                                                                  
000880*----Landeskennung AG         (konstant)               -----*17   
000890     05 DRVB0598-AUFGEB-LAND                   PIC  X(003).       
000900                                                                  
000910*----PLZ Auftraggeber         (konstant)               -----*18   
000920     05 DRVB0598-AUFGEB-PLZ                    PIC  X(010).       
000930                                                                  
000940*----ORT Auftraggeber   kein Pflichtfeld (bleibt leer )-----*19   
000950     05 DRVB0598-AUFGEB-ORT                    PIC  X(030).       
000960                                                                  
000970*----Finanzintern bleibt leer                          -----*20   
000980     05 DRVB0598-INTERN                        PIC  X(011).       
000990                                                                  
001000*----Jahr auf dass sich die Meldungen beziehen         -----*21   
001010     05 DRVB0598-JAHR                          PIC  9(004).       
001020                                                                  
001030*----Anzahl der Mitteilungen                           -----*22   
001040     05 DRVB0598-SUM-MITTEILG                  PIC  9(007).       
001050                                                                  
001060*----Anzahl der Mitteilungen der aktuelle Übermittlung -----*23   
001070     05 DRVB0598-ANZ-MITTEILG                  PIC  9(007).       
001080                                                                  
001090*----reserviert bleibt leer                            -----*24   
001100     05 DRVB0598-RESERVE                       PIC  X(770).       

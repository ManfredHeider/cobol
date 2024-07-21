000010*---------------------------------------------------------------* 
000020*    Standard für alle Objekte unter PVCS                         
000030*            Datum   :  Januar 2001                               
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
000050*---------------------------------------------------------------* 
000060*                                                                 
000070*---------------------------------------------------------------* 
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH                    
000090*---------------------------------------------------------------* 
000100*    $Workfile:   drvb0599.cpy  $                                 
000110*    $Revision:   2.1  $                                          
000120*                                                                 
000130*    $Log:   L:/mf/cpy/DRVB0599.CPv  $                                
000140*
000150*   Rev 2.1   Apr 22 2003 16:19:20   t13ae35
000160*AVB00618 - neu, Satzbeschreibung Meldungssatz 
000170*(109A ESTG)
000180*                                                                 
000190*   Rev 2.0   Feb 25 2002 17:34:04   p13sp20                      
000200*Archiverstellung                                                 
000210*                                                                 
000220*---------------------------------------------------------------* 
000230****************************************************************  
000240*                                                              *  
000250*   COPY-MEMBER: beschreibt die Satzstruktur der Meldungssätze *  
000260*                für die österr. Finanzbehörde                 *  
000270*--------------------------------------------------------------*  
000280* AENDERUNGEN:                                                 *  
000290* AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
000300* --------+--------+----+---------------------------+----------*  
000310* AVB00618!04.04.03!2.1 ! NEU                       ! J.Hering *  
000320* --------+--------+----+---------------------------+----------*  
000330*         !        !    !                           !          *  
000340****************************************************************  
000350*                                                                 
000360 01  DRVB0599-MELDSATZ.                                           
000370                                                                  
000380*----Satzart (konstant)                                -----*01   
000390     05 DRVB0599-SA                         PIC  X(001).          
000400                                                                  
000410*----finanzintern (bleibt leer )                       -----*02   
000420     05 DRVB0599-ADRESSAT                   PIC  X(006).          
000430                                                                  
000440*----finanzintern (bleibt leer )                       -----*03   
000450     05 DRVB0599-DVR-NR1                    PIC  X(007).          
000460                                                                  
000470*----Finanzamtsnummer kein Pflichtfeld (bleibt leer )  -----*04   
000480     05 DRVB0599-FINAMT-NR1.                                      
000490        10 DRVB0599-FINAMT-NR1-NUM          PIC  9(002).          
000500                                                                  
000510*----Steuernummer MS  kein Pflichtfeld (bleibt leer )  -----*05   
000520     05 DRVB0599-STEUER-NR1.                                      
000530        10 DRVB0599-STEUER-NR1-NUM          PIC  9(007).          
000540                                                                  
000550*----DVR-Nummer   MS  kein Pflichtfeld (bleibt leer )  -----*06   
000560     05 DRVB0599-DVR-NR2.                                         
000570        10 DRVB0599-DVR-NR2-NUM             PIC  9(007).          
000580                                                                  
000590*----Art der Übermittlung (bleibt leer)                -----*07   
000600     05 DRVB0599-ART                        PIC  X(001).          
000610                                                                  
000620*----Datum der Übermittlung (wir bei Erst. ermittelt)  -----*08   
000630     05 DRVB0599-DATUM                      PIC  9(008).          
000640                                                                  
000650*----Uhrzeit d. Übermittlung kein Pflichtfeld (bleibt leer )*09   
000660     05 DRVB0599-UHRZEIT.                                         
000670        10 DRVB0599-UHRZEIT-NUM             PIC  9(006).          
000680                                                                  
000690*----VB-Nummer + Jahr der Meldungssätze                   --*10   
000700     05 DRVB0599-REF-NR                     PIC  X(030).          
000710                                                                  
000720*----finanzintern (bleibt leer )                       -----*11   
000730     05 DRVB0599-INTERN                     PIC  X(002).          
000740                                                                  
000750*----finanzintern (bleibt leer )                       -----*12   
000760     05 DRVB0599-FEHLER-MELD.                                     
000770        10 DRVB0599-FEHLER-MELD1            PIC  X(004).          
000780        10 DRVB0599-FEHLER-MELD2            PIC  X(004).          
000790        10 DRVB0599-FEHLER-MELD3            PIC  X(004).          
000800        10 DRVB0599-FEHLER-MELD4            PIC  X(004).          
000810        10 DRVB0599-FEHLER-MELD5            PIC  X(004).          
000820                                                                  
000830*----finanzintern (bleibt leer )                       -----*13   
000840     05 DRVB0599-STATUS                     PIC  X(001).          
000850                                                                  
000860*----finanzintern (bleibt leer )                       -----*14   
000870     05 DRVB0599-FEHLER-IND                 PIC  X(001).          
000880                                                                  
000890*----Jahr auf dass sich die Meldungen beziehen         -----*15   
000900     05 DRVB0599-JAHR                       PIC  9(004).          
000910                                                                  
000920*----lfd. Nummer kein Pflichtfeld (bleibt leer )       -----*16   
000930     05 DRVB0599-LFD-NR.                                          
000940        10 DRVB0599-LFD-NR-NUM              PIC  X(004).          
000950                                                                  
000960*----Geburtsdatum im Format TTMMJJ                     -----*17   
000970     05 DRVB0599-VB-GEBDAT                  PIC  9(006).          
000980                                                                  
000990*----Finanzamtsnummer kein Pflichtfeld (bleibt leer )  -----*18   
001000     05 DRVB0599-FINAMT-NR2.                                      
001010        10 DRVB0599-FINAMT-NR2-NUM          PIC  9(002).          
001020                                                                  
001030*----Steuernummer     kein Pflichtfeld (bleibt leer )  -----*19   
001040     05 DRVB0599-STEUER-NR2.                                      
001050       10 DRVB0599-STEUER-NR2               PIC  9(007).          
001060                                                                  
001070*----Name + Vorname                                    -----*20   
001080     05 DRVB0599-VB-NAME                    PIC  X(037).          
001090                                                                  
001100*----Titel            kein Pflichtfeld (bleibt leer )  -----*21   
001110     05 DRVB0599-VB-TITEL                   PIC  X(020).          
001120                                                                  
001130*----Strasse                                           -----*22   
001140     05 DRVB0599-VB-STRASSE                 PIC  X(037).          
001150                                                                  
001160*----Landeskennung der Adresse                         -----*23   
001170     05 DRVB0599-VB-LAND                    PIC  X(003).          
001180                                                                  
001190*----PLZ                                               -----*24   
001200     05 DRVB0599-VB-PLZ                     PIC  X(010).          
001210                                                                  
001220*----Ort              kein Pflichtfeld (bleibt leer )  -----*25   
001230     05 DRVB0599-VB-ORT                     PIC  X(030).          
001240                                                                  
001250*----Art der Leistung (konstant)                       -----*26   
001260     05 DRVB0599-LSTG-ART                   PIC  X(002).          
001270                                                                  
001280*----Vorzeichen des Verdienstes                        -----*27   
001290     05 DRVB0599-VZ1                        PIC  X(001).          
001300                                                                  
001310*----Verdienst ohne VZ                                 -----*28   
001320     05 DRVB0599-ENTGELT                    PIC  9(08)V99.        
001330                                                                  
001340*----Vorzeichen der Umsatzsteuer (bleibt leer)         -----*29   
001350     05 DRVB0599-VZ2                        PIC  X(001).          
001360                                                                  
001370*----Umsatzsteuer   (bleibt leer)                      -----*30   
001380     05 DRVB0599-UST                        PIC  9(08)V99.        
001390                                                                  
001400*----Finanzamtsnummer (konstant)                       -----*31   
001410     05 DRVB0599-FINAMT-NR3                 PIC  9(002).          
001420                                                                  
001430*----Steuernummer AG  (konstant)                       -----*32   
001440     05 DRVB0599-STEUER-NR3                 PIC  9(007).          
001450                                                                  
001460*----DVR-Nummer   AG  kein Pflichtfeld (aber koonst.)  -----*33   
001470     05 DRVB0599-DVR-NR3.                                         
001480        10 DRVB0599-DVR-NR3-NUM             PIC  9(007).          
001490                                                                  
001500*----Tel-Nummer   VB  kein Pflichtfeld (aber koonst.)  -----*34   
001510     05 DRVB0599-VB-TEL-NR.                                       
001520        10 DRVB0599-VB-TEL-NR-NUM-T1        PIC  9(010).          
001530        10 DRVB0599-VB-TEL-NR-NUM-T2        PIC  9(010).          
001540                                                                  
001550*----Fax-Nummer   VB  kein Pflichtfeld (aber koonst.)  -----*35   
001560     05 DRVB0599-VB-TELEFAX.                                      
001570        10 DRVB0599-VB-TELEFAX-NUM-T1       PIC  9(010).          
001580        10 DRVB0599-VB-TELEFAX-NUM-T2       PIC  9(010).          
001590                                                                  
001600*----Korrektur-Nummer (Blank bei Erstüberm. sonst K )  -----*36   
001610     05 DRVB0599-KORR-INDIKATOR             PIC  X(001).          
001620                                                                  
001630*----Reserver-Felder (bleiben leer)                    -----*37   
001640     05 DRVB0599-RESERVE                    PIC  X(660).          

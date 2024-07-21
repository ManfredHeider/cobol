      *****************************************************************
      *    COPY-MEMBER: DRKI1950                                     
      *                                                              
      *    G50/GEDKranken DTA Rückstellungswerte
      *    Satzlänge: 100 Bytes                                      
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DBO-9273   !05.10.21! NEU                          ! T13AC20  *
      * -----------!--------!------------------------------!----------*
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
           05 G50-SATZ0                           PIC  X(100).       
      *                                                             
      ** Personensatz 'D201'.                                        
      ** Kann pro Vertrag 1:n vorhanden sein. Es gibt je Person     
      ** jedoch nur einen Personensatz.                             
           05 G50-SATZ2 REDEFINES G50-SATZ0.                        
              10 G50-SA2-KEY.                                       
      ** Satzart; fix 'D'                                           
                 15 G50-SA2-SA                    PIC  X(001).      
      ** Datensatzart; fix 201                                      
                 15 G50-SA2-DATENSATZART          PIC  X(003).      
      ** Vertragsnummer                                             
                 15 G50-SA2-VTRGNR-X.       
                    20 G50-SA2-VTRGNR             PIC  9(009).      
              10 G50-SA2-PERSONENSATZ.                              
      ** Personennummer der versicherten Person (VP)                
                 15 G50-SA2-VPNR-X.       
                    20 G50-SA2-VPNR               PIC  9(004).      
      ** Rückstellungswerte einer Person                            
                 15 G50-SA2-RUECKSTELLUNGSWERTE.                    
      ** Altersrueckstellung                                        
                    20 G50-SA2-ALTRUECKSTELLG-X.   
                       25 G50-SA2-ALTRUECKSTELLG  PIC S9(007)V99.   
      ** Beitragsvorteil                                            
                    20 G50-SA2-BEITRAGSVORTEIL-X.    
                       25 G50-SA2-BEITRAGSVORTEIL PIC S9(007)V99.   
      ** Stand Rückstellungswerte in der Form TTMMJJJJ              
                    20 G50-SA2-STAND-X.
                       25 G50-SA2-STAND           PIC  9(008).      
      ** FILLER                                                     
              10                                  PIC  X(057).      
      *                                                             
      ** DTA Vorsatz                                                
           05 G50-SATZA  REDEFINES G50-SATZ0.                       
      ** Satzart; fix 'A'                                           
              10 G50-SAA-SA                       PIC  X(001).      
      ** Absender; fix 'GENERALI DEU KRANKEN'                       
              10 G50-SAA-ABSENDER                 PIC  X(027).      
      ** Erstellungsdatum in der Form TTMMJJJJ                      
              10 G50-SAA-ERSTELLDAT               PIC  9(008).      
      ** Stichtag in der Form TTMMJJJJ                              
              10 G50-SAA-DTASTAND                 PIC  9(008).      
      ** Empfänger 'DVAG', 'ALLF oder 'ALAG'                        
              10 G50-SAA-EMPFAENGER               PIC  X(004).      
      ** Inhalt des DTAs; fix 'RUECKSTELLUNGSWERTE'                 
              10 G50-SAA-INHALT                   PIC  X(020).      
      ** FILLER                                                     
              10                                  PIC  X(032).      
      *                                                             
      ** DTA Nachsatz                                               
           05 G50-SATZE REDEFINES G50-SATZ0.                        
      ** Satzart; fix 'E'                                           
              10 G50-SAE-SA                       PIC  X(001).      
      ** Anzahl Datensätze mit Satzart 'D'                          
              10 G50-SAE-ANZSATZ                  PIC  9(009).      
      ** Anzahl Datensätze mit Satzart 'D' / Datensatzart '201'     
              10 G50-SAE-ANZSATZ-DSA201           PIC  9(009).      
      ** FILLER                                                     
              10                                  PIC  X(081).      
      *****************************************************************
      *---------------------- ENDE DRKI1950 --------------------------* 
      *****************************************************************

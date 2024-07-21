      *****************************************************************
      *    COPY-MEMBER: DRKI3914                                     
      *                                                              
      *    G50/GDK DTA Rückstellungswerte                             
      *    Aktualisierungsdatei für DB2 Tabelle KRANKENPERSEIGEN
      *                                                              
      *                                                              
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DBO-9273   !05.11.21! NEU ERSTELLT                 ! U131382  *
      * -----------!--------!------------------------------!----------*
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
           05 DRKI3914-SATZ0                   PIC  X(80).       
      ** Kann pro Vertrag 1:n vorhanden sein. Es gibt je Person     
      ** jedoch nur einen Personensatz.                             
           05 FILLER REDEFINES DRKI3914-SATZ0.                        
      ** Vertragsnummer                                             
              10 DRKI3914-VTRGNR               PIC  X(015).       
      ** Vertrags-ID                
              10 DRKI3914-VTRGID               PIC  X(013).       
      ** Personennummer der versicherten Person (VP)                
              10 DRKI3914-VPNR                 PIC  9(004).       
      ** Rückstellungswerte einer Person                            
              10 DRKI3914-ALTRUECKSTELLG       PIC S9(007)V99.    
      ** Beitragsvorteil                                            
              10 DRKI3914-BEITRAGSVORTEIL      PIC S9(007)V99.    
      ** Stand Rückstellungswerte in der Form JJJJ-MM-TT             
              10 DRKI3914-STAND                PIC  X(010).       
      ** Aktualisierungs-Kennzeichen:         
      *          I = INSERT                                
      *          U = UPDATE                                
      *          D = DELETE                                
              10 DRKI3914-AKTUAL-KZ            PIC  X(001).    
      ** FILLER                                                     
              10 FILLER                        PIC  X(019).       
      *                                                             
      *****************************************************************
      *---------------------- ENDE DRKI3914 --------------------------* 
      *****************************************************************

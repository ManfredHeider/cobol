      ******************************************************************        
      *                                                                *        
      *    COPY-MEMBER für die Ausgabedatei Änderungsbestand           *      
      *                Kontokorrentdaten von der Deutschen Bank        *        
      *                                                                *        
      *    SATZLÄNGE 200                                               *       
      *                                                                *        
      *    LOGBUCH                                                     *        
      *****************************************************************         
      *****************************************************************         
      * LOGBUCH:                                                      *         
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *         
      * --------------------------------------------------------------*         
      * DBO-8784   !22.07.21! NEU                          ! WHO      *         
      *            !        !                              !          *         
      *            !        !                              !          *         
      *                                                               *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      *                                                                         
       01  DRKI3895.                                                            
      *                                                                         
           05  BNK-SA                        PIC  X(002).                       
      *            SATZART        = '10'                                        
           05  BNK-GES                       PIC  9(002).                       
      *            GESELLSCHAFT   = '48'                                        
           05  BNK-KEY.                                                         
               10  BNK-VNR                   PIC  9(015) .                     
      *                                                                         
           05  BNK-VERARB-KZ                 PIC  X(001).                       
      *            VERARBEITUNGSKENNZEICHEN                                     
      *                I = INSERT                                               
      *                U = UPDATE                                               
      *                                                                         
           05  BNK-DATUM-EROEFFNUNG.                                            
               10 BNK-JAHR-EROEFFNUNG        PIC  9(004).                       
               10 BNK-MONAT-EROEFFNUNG       PIC  9(002).                       
               10 BNK-TAG-EROEFFNUNG         PIC  9(002).                       
           05  BNK-AEND-DAT-E-KZ             PIC  X(001).                       
      *        J = Änderung Datum Eroeffnung                                   
      *                                                                         
           05  BNK-DATUM-SCHLIESSUNG.                                           
               10 BNK-JAHR-SCHLIESSUNG       PIC  9(004).                       
               10 BNK-MONAT-SCHLIESSUNG      PIC  9(002).                       
               10 BNK-TAG-SCHLIESSUNG        PIC  9(002).                       
           05  BNK-AEND-DAT-S-KZ             PIC  X(001).                       
      *        J = Änderung Datum Schliessung                                  
      *                                                                         
           05  BNK-BILANZSCHLUESSEL          PIC  X(004).                      
           05  BNK-AEND-BILANZSCHLUESSEL-KZ  PIC  X(001).                       
      *        J = Änderung Bilanzschlüssel                                   
      *                                                                         
           05  BNK-EXTERNES-LIMIT            PIC  9(012)V99.                    
           05  BNK-AEND-EXTERNES-LIMIT-KZ    PIC  X(001).                       
      *        J = Änderung Externes Limit                                     
      *                                                                         
           05  BNK-SALDO                     PIC  S9(012)V99.                   
           05  BNK-AEND-SALDO-KZ             PIC  X(001).                       
      *        J = Änderung Saldo                                              
      *                                                                         
           05  BNK-KONDITIONSSCHLUESSEL      PIC  X(002).                       
           05  BNK-AEND-KOND-SCHLUESSEL-KZ   PIC  X(001).                       
      *        J = Änderung Konditionsschlüssel                               
      *                                                                         
           05  BNK-KUMHABENAM                PIC  S9(012)V99.                   
           05  BNK-AEND-KUMHABENAM-KZ        PIC  X(001).                       
      *        J = Änderung Habenumsätze aktueller Monat                      
           05  BNK-KUMSOLLAM                 PIC  S9(012)V99.                   
           05  BNK-AEND-KUMSOLLAM-KZ         PIC  X(001).                       
      *        J = Änderung Sollumsätze aktueller Monat                       
           05  BNK-KUMHABENVM                PIC  S9(012)V99.                   
           05  BNK-AEND-KUMHABENVM-KZ        PIC  X(001).                       
      *        J = Änderung Habenumsätze Vormonat                             
           05  BNK-KUMSOLLVM                 PIC  S9(012)V99.                   
           05  BNK-AEND-KUMSOLLVM-KZ         PIC  X(001).                       
      *        J = Änderung Sollumsätze Vormonat                              
           05  BNK-ZRAM                      PIC  X(005).                       
           05  BNK-AEND-ZRAM-KZ              PIC  X(001).                       
      *        J = Änderung Monat für aktuelle Umsätze                       
           05  BNK-ZRVM                      PIC  X(005).                       
           05  BNK-AEND-ZRVM-KZ              PIC  X(001).                       
      *        J = Änderung Monat für Vormonats-Umsätze                      
           05  BNK-SOLLZINS                  PIC  S9(02)V9999.                  
           05  BNK-AEND-SOLLZINS-KZ          PIC  X(001).                       
      *        J = Änderung Sollzinssatz                                       
           05  BNK-UEBERZZINS                PIC  S9(02)V9999.                  
           05  BNK-AEND-UEBERZZINS-KZ        PIC  X(001).                       
      *        J = Änderung Überuiehungszinssatz                              
           05  BNK-HABENZINS                 PIC  S9(02)V9999.                  
           05  BNK-AEND-HABENZINS-KZ         PIC  X(001).                       
      *        J = Änderung Habenzinssatz                                      
           05  BNK-RESERVE                   PIC X(034).                        
      *        Reserve                                                          
                                                                                
      *****************************************************************         
      *---------------------- ENDE XXXX0000 --------------------------*         
      *****************************************************************         

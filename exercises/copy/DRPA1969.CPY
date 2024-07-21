      *****************************************************************
      *    COPY-MEMBER: DRPA1969                                     
      *                                                              
      *   <KURZE FACHLICHE BESCHREIBUNG>                             
      *    Dieses Copy matcht die Empfängertypen des DRAT0117 (ohne VTF
      *    und VWF) zu Eigen- oder Gruppengeschäft:
      *
      *    0 => Gruppengeschäft
      *    1 => Eigengeschäft
      *    9 => nicht relevant
      *                                                              
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DAISY-4437 !21.04.22! NEU                          ! KÖPRÜCÜ  *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
        01 DRPA1969.
           05 K-EIGEN-EMP-TYP.
              10 K-EIGEN-VTF            PIC S9(004) BINARY  VALUE  9. 
              10 K-EIGEN-VWF            PIC S9(004) BINARY  VALUE  9. 
              10 K-EIGEN-VM             PIC S9(004) BINARY  VALUE  0.  
              10 K-EIGEN-KD-BETR        PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-BETREUER       PIC S9(004) BINARY  VALUE  0.  
              10 K-EIGEN-NG-STRUK       PIC S9(004) BINARY  VALUE  0.  
              10 K-EIGEN-AVERM          PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-AVERM-O-P      PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-AVERM-P2       PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-AVERM-P3       PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-AVERM-P4       PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-AVERM-P5       PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-AVERM-P6       PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-AVERM-P7       PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-AVERM-ALT      PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-VBUE           PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-VBUE-ALT       PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-KDUE           PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-KUEF           PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-QUER           PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-OAE            PIC S9(004) BINARY  VALUE  0.  
              10 K-EIGEN-N1EM           PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-N1EM-ALT       PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-N1EO           PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-N1EO-ALT       PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-N2EO           PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-N2EO-ALT       PIC S9(004) BINARY  VALUE  1.  
              10 K-EIGEN-N1GM           PIC S9(004) BINARY  VALUE  0.  
              10 K-EIGEN-N1GO           PIC S9(004) BINARY  VALUE  0.  
              10 K-EIGEN-N2GM           PIC S9(004) BINARY  VALUE  0.  
              10 K-EIGEN-N2GO           PIC S9(004) BINARY  VALUE  0.  
      *
           05 K-EIGEN-EMP-TYP-TABELLE REDEFINES K-EIGEN-EMP-TYP.        
              10 K-EIGEN-EMP-TYP-ZEILE  OCCURS 31                       
                            INDEXED BY I-EIGEN-EMP-TYP.          
                 15 K-EIGEN-EMP-TYP-WERT                                
                            PIC S9(004) BINARY.
                    88 K-EIGEN-EMP-TYP-NEIN         VALUE 0.
                    88 K-EIGEN-EMP-TYP-JA           VALUE 1. 
                    88 K-EIGEN-EMP-TYP-NOT-R        VALUE 9.

      *****************************************************************
      *---------------------- ENDE DRPA1969 --------------------------* 
      *****************************************************************
